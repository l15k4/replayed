package com.pragmaxim.replayed

import com.pragmaxim.idb._
import com.pragmaxim.replayed.event.{NewDoc, NewOrLast}
import com.pragmaxim.replayed.mvc.{ReplayDispatcher, EditorComponent, MarkdownComponent, NavbarComponent}
import monifu.concurrent.Scheduler
import scala.scalajs.js.Dynamic.{literal => lit}
import scala.scalajs.js.JSApp
import org.scalajs.dom.console
import com.pragmaxim.idb.IdbSupport._
import upickle.legacy._
import IOUtils._

import scala.util.{Failure, Success}

case class Document(title: String, created: Double, edited: Double)

object Replayed extends JSApp {

  val docStoreName = "documents"
  val editorDbName = "editor"

  val docTitleIdx = "title"
  val docCreatedIdx = "created"
  val docEditedIdx = "edited"

  def main(): Unit = {
    implicit val scheduler = Scheduler.trampoline()
    val asyncScheduler = Scheduler()

    val sampleDataFuture = readRemoteResource("http://pragmaxim.github.io/replayed/README.json")

    val idb = IndexedDb(
      OpenDb(editorDbName,
        Some { (db, ve) =>
          val docStore = db.createObjectStore(docStoreName, lit("keyPath" -> docCreatedIdx))
          docStore.createIndex(docTitleIdx, docTitleIdx)
          docStore.createIndex(docCreatedIdx, docCreatedIdx)
          docStore.createIndex(docEditedIdx, docEditedIdx)
          ()
        }
      )
    )

    val dispatcher = ReplayDispatcher { channel =>
      List(
        NavbarComponent(channel, idb),
        EditorComponent(channel, idb),
        MarkdownComponent(channel)(asyncScheduler)
      )
    }

    val docStore = idb.openStore[Double, Document](docStoreName)
    docStore.count.doWorkOnSuccess { counts =>
      if (counts.head > 0) {
        dispatcher.pushNext(NewOrLast)
      } else {
        sampleDataFuture.map(unmarshal).onComplete {
          case Success(changes) =>
            dispatcher.pushNext(NewDoc(title = "documentation", data = Some(changes)))
          case Failure(ex) =>
            console.warn("Unable to load sample data " + ex.getMessage)
            dispatcher.pushNext(NewOrLast)
        }
      }
    }.subscribe()
  }

}