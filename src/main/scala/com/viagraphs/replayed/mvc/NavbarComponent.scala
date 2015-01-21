package com.viagraphs.replayed.mvc

import com.viagraphs.idb.{Direction, UpgradeDb, IndexedDb}
import com.viagraphs.replayed.Document
import com.viagraphs.replayed._
import com.viagraphs.replayed.event._
import monifu.concurrent.Scheduler
import monifu.reactive.Ack
import monifu.reactive.Ack.Continue
import monifu.reactive.channels.SubjectChannel
import com.viagraphs.idb.IdbSupport._
import org.scalajs.dom.{MouseEvent, document, window, Event, KeyboardEvent}
import org.scalajs.dom.html.Div
import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{literal => lit}
import scala.concurrent.Future
import scala.util.{Failure, Success, Random}
import scalatags.JsDom.all._
import upickle._
import com.viagraphs.replayed.IOUtils._

class NavbarComponent(val channel: SubjectChannel[RxEvent, RxEvent], idb: IndexedDb)(implicit s: Scheduler) extends UiComponent {
  val componentName = "navbar"

  val store = idb.openStore[Double, Document](Replayed.docStoreName)
  val editedIndex = store.index[Double]("edited")
  val createdIndex = store.index[Double]("created")

  val ne = document.getElementById("e_navbar").asInstanceOf[Div]

  def onSubscribe(): Unit = {}

  private def exportJson(storeName: String, onDone: (Int) => Unit, onNextRecord: (Int, Change) => Unit): Unit = {
    val curDocStore = idb.openStore[Int,Change](storeName)
    curDocStore.count.doWorkOnSuccess { counts =>
      val count = counts(0)
      curDocStore.get(curDocStore.allKeys(Direction.Next))
        .doOnComplete(onDone(count))
        .foreach { case (key, record) =>
          val percent = (key * 100) / count
          onNextRecord(percent, record)
        }
    }.subscribe()
  }

  def render(currentDoc: Document, allDocs: List[Document], warning: Option[String]): Unit = {

    def measureTitle(value: String) =
      (measure(value, "document-title tmp-document-title") + 30) + "px"

    def exportAnchor(suffix: String) =
      a("download".attr := s"${currentDoc.title}.$suffix")(
        div(
          span(`class` := "dropdown-item-title")(suffix)
        )
      ).render

    def importAnchor(modifiers: Modifier*) =
      a(href := "#")(
        div(
          span(`class` := "dropdown-item-title")(modifiers)
        )
      ).render

    val errorField =
       div(`class` := "alert alert-danger", role := "alert")(
         span(`class` := "glyphicon glyphicon-exclamation-sign")
       ).render

    warning match {
      case Some(msg) =>
        import scala.concurrent.duration._
        errorField.textContent = msg
        s.scheduleOnce(
          3.seconds,
          errorField.style.visibility = "hidden"
        )
      case None => errorField.style.visibility = "hidden"
    }

    def importDocFrom(resource: Future[String], title: String = "untitled"): Unit =
      resource
        .map(unmarshal)
        .onComplete {
        case Success(changes) =>
          channel.pushNext(NewDoc(title, data = Some(changes)))
        case Failure(ex) =>
          render(currentDoc, allDocs, Some(s"We are sorry, importing document failed due to : ${ex.getMessage}"))
      }

    val jsonExportAnchor = exportAnchor("json")
    jsonExportAnchor.onclick = (e: MouseEvent) => {
      val titleSpan = jsonExportAnchor.getElementsByClassName("dropdown-item-title")(0)
      val changes = ArrayBuffer[Change]()
      var percent = 0
      exportJson(
        currentDoc.created.toString,
        (count) => {
          titleSpan.textContent = s"download"
          val result = write[ArrayBuffer[Change]](changes)
          jsonExportAnchor.href = "data:application/octet-stream;base64," + window.btoa(result)
          jsonExportAnchor.onclick = (e: Event) => render(currentDoc, allDocs, None)
        },
        (percentage, change) => {
          changes.append(change)
          if (percent != percentage) {
            percent = percentage
            titleSpan.textContent = s"exporting $percentage%"
          }
        }
      )
    }

    val htmlExportAnchor = exportAnchor("html")
    htmlExportAnchor.onclick = (e: MouseEvent) => {
      val em = document.getElementById("e_markdown")
      htmlExportAnchor.href = "data:application/octet-stream;base64," + window.btoa(em.innerHTML)
      htmlExportAnchor.onclick = (e: Event) => render(currentDoc, allDocs, None)
    }

    val importUrlInput = input(`type` := "text", placeholder := "Url", `class` := "form-control input-sm").render
    importUrlInput.onkeydown = (e: KeyboardEvent) => {
      if (e.keyCode == 13 && !importUrlInput.value.isEmpty)
        importDocFrom(readRemoteResource(importUrlInput.value))
    }
    val importUrlAnchor = importAnchor("Remote file ", importUrlInput)
    importUrlAnchor.onclick = (e: MouseEvent) => {
      if (!importUrlInput.value.isEmpty)
        importDocFrom(readRemoteResource(importUrlInput.value))
    }

    val importFileInput = input(`type` := "file").render
    val importFileAnchor = importAnchor(importFileInput, "Local file")
    importFileAnchor.onchange = (e: Event) => {
      val file = importFileInput.files(0)
      val extDotIdx = file.name.lastIndexOf('.')
      val fileName = if (extDotIdx > 0) file.name.substring(0, extDotIdx) else file.name
      importDocFrom(readFile(file), fileName)
    }

    val titleInput = input(`type` := "text", `class` := "form-control input-sm", placeholder := "Title", value := currentDoc.title).render
    titleInput.style.width = measureTitle(titleInput.value)
    titleInput.oninput = (e: Event) =>
      channel.pushNext(StoreDoc(currentDoc.copy(title = titleInput.value, edited = new js.Date().getTime())))
    titleInput.onkeyup = (e: Event) =>
      titleInput.style.width = measureTitle(titleInput.value)

    val deleteDocSpan = span(`class` := "glyphicon-text", "delete").render

    var holdingDelete = (0, false)

    ne.removeAllChildren()
    ne.appendChild(
      ul(`class` := "nav navbar-nav",
        li(`class` := "navbar-item",
          a(href := "#", onclick := {() => channel.pushNext(NewDoc(data = None))})(
            span(`class` := "glyphicon glyphicon-plus",
              span(`class` := "glyphicon-text", "new")
            )
          )
        ),
        li(`class` := "navbar-item dropdown",
          a(`class` := "dropdown-toggle", href := "#", "data-toggle".attr := "dropdown", onclick := dropdown(toggle,"dropdown","open"))(
            span(`class` := "glyphicon glyphicon-file",
              span(`class` := "glyphicon-text", "open")
            )
          ),
          ul(`class` := "dropdown-menu", role := "menu",
            if (allDocs.isEmpty) {
              li(`class` := "divider")
            } else {
              for (aDoc@Document(title,created,edited) <- allDocs) yield
              li(
                a(href := "#",
                  onclick := {
                    () => {
                      channel.pushNext(OpenDoc(aDoc))
                    }
                  }
                )(
                  div(
                    span(`class` := "dropdown-item-time")(Some(new js.Date(created)).map(now => now.toLocaleDateString() + " at " + now.toLocaleTimeString()).get)
                  ),
                  div(
                    span(`class` := "dropdown-item-title")(title)
                  )
                )
              )
            }
          )
        ),
        li(`class` := "navbar-item dropdown",
          a(`class` := "dropdown-toggle", href := "#", "data-toggle".attr := "dropdown", onclick := dropdown(toggle,"dropdown","open"))(
            span(`class` := "glyphicon glyphicon-export",
              span(`class` := "glyphicon-text", "export")
            )
          ),
          ul(`class` := "dropdown-menu", role := "menu",
            li(jsonExportAnchor),
            li(htmlExportAnchor)
          )
        ),
        li(`class` := "navbar-item dropdown",
          a(`class` := "dropdown-toggle", href := "#", "data-toggle".attr := "dropdown", onclick := dropdown(toggle,"dropdown","open"))(
            span(`class` := "glyphicon glyphicon-import",
              span(`class` := "glyphicon-text", "import")
            )
          ),
          ul(`class` := "dropdown-menu", role := "menu",
            li(importUrlAnchor),
            li(importFileAnchor)
          )
        ),
        li(`class` := "navbar-item",
          a(href := "#",
            onmouseover := {() => deleteDocSpan.textContent = "hold"},
            onmouseout := {() => deleteDocSpan.textContent = "delete"},
            onmouseup := {() =>
              holdingDelete = (0, false)
              deleteDocSpan.textContent = "delete"
            },
            onmousedown := {() =>
              import scala.concurrent.duration._
              val uid = Random.nextInt()
              holdingDelete = (uid, true)
              countDown(3, 600.millis)
                .take(4)
                .doOnComplete {
                  holdingDelete match {
                    case (`uid`, true) =>
                      holdingDelete = (0, false)
                      deleteDocSpan.textContent = "deleted"
                      channel.pushNext(DeleteDoc(currentDoc))
                    case _ =>
                  }
                }.foreach { tick =>
                  holdingDelete match {
                    case (`uid`, true) => deleteDocSpan.textContent = s"hold ${tick.toString}s"
                    case _ =>
                  }
                }
            }
          )(
            span(`class` := "glyphicon glyphicon-remove", deleteDocSpan)
          )
        ),
        li(`class` := "navbar-item document-title",
          titleInput
        ),
        li(`class` := "navbar-item warning",
          errorField
        )
/*
        li(
          a(href := "#", onclick := {() => channel.pushNext(ShareDoc)})(
            span(`class` := "glyphicon glyphicon-user")
          )
        ),
        li(
          a(href := "#", onclick := {() => channel.pushNext(SaveDoc)})(
            span(`class` := "glyphicon glyphicon-save")
          )
        )
*/
      ).render
    )
    Lines.LinesTop = ne.getBoundingClientRect().bottom
  }

  def onNextSafe(elem: RxEvent): Future[Ack] = elem match {
    case NewDoc(docTitle, created, _) =>
      idb.getVersion.onCompleteNewTx { versions =>
        idb.upgrade(
          UpgradeDb(
            Replayed.editorDbName,
            versions(0) + 1,
            Some { (db, ve) =>
              db.createObjectStore(created.toString, lit("autoIncrement" -> true))
              ()
            }
          )
        )
      }.asCompletedFuture.flatMap { continue =>
        val newDoc = Document(docTitle, created, created)
        editedIndex.get(editedIndex.allKeys(Direction.Next)).onCompleteNewTx { allDocs =>
          store.add(List(newDoc)).doOnComplete {
            render(newDoc, allDocs.unzip._2.toList, None)
          }
        }.asCompletedFuture
      }
    case OpenDoc(aDoc) =>
      createdIndex.get(createdIndex.allKeys(Direction.Next)).doWorkOnSuccess { allDocs =>
        val toBeListed =
          allDocs.filter { case (created,doc) =>
            created != aDoc.created
          }.unzip._2.toList
        render(aDoc, toBeListed, None)
      }.asCompletedFuture
    case StoreDoc(aDoc) =>
      store.update(List(aDoc)).asCompletedFuture
    case DeleteDoc(aDoc) =>
      store.delete(List(aDoc.created)).doOnComplete {
        channel.pushNext(NewOrLast)
      }.asCompletedFuture
    case NewOrLast =>
      createdIndex.get(createdIndex.lastKey()).doWorkOnSuccess { lastCol =>
        lastCol.headOption match {
          case Some((created, last)) =>
            channel.pushNext(OpenDoc(last))
          case None =>
            channel.pushNext(NewDoc(data = None))
        }
      }.asCompletedFuture
    case _ => Continue
  }

  def onComplete(): Unit = {}
}
object NavbarComponent {
  def apply(channel: SubjectChannel[RxEvent, RxEvent], idb: IndexedDb)(implicit s: Scheduler) = new NavbarComponent(channel, idb)
}

/**

<form class="navbar-form navbar-left" role="search">
  <div class="form-group">
    <input type="text" class="form-control" placeholder="Search">
  </div>
  <button type="submit" class="btn btn-default">Submit</button>
</form>

<a href="#">Inbox <span class="badge">42</span></a>
<button class="btn btn-primary" type="button">
  Messages <span class="badge">4</span>
</button>

<span class="label label-default">Default</span>
<span class="label label-primary">Primary</span>
<span class="label label-success">Success</span>
<span class="label label-info">Info</span>
<span class="label label-warning">Warning</span>
<span class="label label-danger">Danger</span>

<div class="alert alert-success" role="alert">...</div>
<div class="alert alert-info" role="alert">...</div>
<div class="alert alert-warning" role="alert">...</div>
<div class="alert alert-danger" role="alert">...</div>

<div class="alert alert-warning alert-dismissible" role="alert">
  <button type="button" class="close" data-dismiss="alert"><span aria-hidden="true">&times;</span><span class="sr-only">Close</span></button>
  <strong>Warning!</strong> Better check yourself, you're not looking too good.
</div>

*/