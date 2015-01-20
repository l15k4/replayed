package com.viagraphs.replayed.mvc

import com.viagraphs.scalajs.dom.KCode
import com.viagraphs.scalajs.dom.KeyboardPolyfill.PfEvent
import com.viagraphs.replayed.event._
import com.viagraphs.replayed.{Navigator, _}
import monifu.concurrent.Scheduler
import monifu.reactive.{Observable, Ack}
import monifu.reactive.Ack.Continue
import monifu.reactive.channels.SubjectChannel
import org.scalajs.dom._
import com.viagraphs.idb.{IndexedDb, Direction, Store}
import com.viagraphs.idb.IdbSupport._
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Promise, Future}
import scala.scalajs.js.UndefOr

trait StashingChangeLog { this: EditorComponent =>
  private val stash = ListBuffer[Change]()

  def stashSize: Int = stash.length

  def commit(change: Change): Observable[(Int,Change)] = {
    if (stash.nonEmpty) {
      stash.clear()
    }
    store.add(change :: Nil)
  }

  def revert(): Observable[(Int,Change)] =
    store.get(store.lastKey()).onCompleteNewTx { lastCol =>
      lastCol.headOption.fold(Observable.empty.asInstanceOf[Observable[(Int,Change)]]) { last =>
        store.delete(store.lastKey()).endWith(last)
      }
    }

  def stash(entry: Change): ListBuffer[Change] = entry +=: stash

  def unStash(): Change = stash.remove(0)

  def stashApply(entry: Change): Observable[(Int,Change)] = store.add(entry :: Nil)
}

class EditorComponent(val channel: SubjectChannel[RxEvent, RxEvent], idb: IndexedDb)(implicit val s: Scheduler) extends Component with StashingChangeLog {
  val store: Store[Int,Change] = idb.openStore[Int,Change]("nonExisting")
  val componentName = "editor"
  val ee = document.getElementById("e_editor").asInstanceOf[HTMLDivElement]

  val le = document.createElement("div").markup[HTMLDivElement] { lines =>
    lines.style.fontSize = "12px"
    lines.className = "e_lines"
    lines.id = "e_lines"
  }
  val pe = document.createElement("div").markup[HTMLDivElement] { lines =>
    lines.className = "e_pointer"
    lines.id = "e_pointer"
  }

  ee.appendChild(le)
  ee.appendChild(pe)

  // window.onbeforeunload = (e: BeforeUnloadEvent) => "Do you really want to leave ?"

  val lines = new Lines(le)
  val meter = Meter(lines)
  val nav = new Navigator(pe, lines, meter)

  def resetEditor(): Unit = {
    lines.reset()
    nav.reset()
  }

  def onSubscribe() = {
    import com.viagraphs.replayed.RichHTMLElement
    def relativeX(pageX: Double) = pageX - Lines.LinesLeft + ee.scrollLeft
    def relativeY(pageY: Double) = ((pageY - Lines.LinesTop) / Lines.LineHeight).toInt

    val onClick = (event: MouseEvent) => {
      val shifted = event.shiftKey
      Option((event.srcElement: UndefOr[Element]).getOrElse(event.target.asInstanceOf[Element])).collect[ClickEvent] {
        case line: HTMLDivElement if line.hasClass("e_line") =>
          ClickEvent(Some(line.childIdx()), relativeX(event.pageX), shifted)
        case editor: HTMLDivElement if editor.hasClass("e_editor") =>
          val lidxOpt = lines.line(relativeY(event.pageY)).map(_.childIdx())
          ClickEvent(lidxOpt, relativeX(event.pageX))
        case lines: HTMLDivElement if lines.hasClass("e_lines") =>
          val lineIdx = relativeY(event.pageY)
          ClickEvent(Some(lineIdx), relativeX(event.pageX), shifted)
        case text: HTMLSpanElement if text.hasClass("e_text") => // This should never occur since e_text has "pointer-events: none"
          ClickEvent(Some(text.parentNode.childIdx()), relativeX(event.pageX), shifted)
      }.foreach { ce =>
        channel.pushNext(ce)
      }
    }

    val onDoubleClick = (event: MouseEvent) => {
      Option((event.srcElement: UndefOr[Element]).getOrElse(event.target.asInstanceOf[Element])).collect[DoubleClickEvent] {
        case line: HTMLDivElement if line.hasClass("e_line") =>
          DoubleClickEvent(line.childIdx(), relativeX(event.pageX))
        case text: HTMLSpanElement if text.hasClass("e_text") => // This should never occur since e_text has "pointer-events: none"
          DoubleClickEvent(text.parentNode.childIdx(), relativeX(event.pageX))
      }.foreach { ce =>
        channel.pushNext(ce)
      }
    }

    val onKeyUp = (event: Event) => {
      val kbe = event.asInstanceOf[KeyboardEvent]
      val ctrl = kbe.ctrlKey
      kbe.polyfill() match {
        case (KCode.V, _) if ctrl =>
          Option((document.activeElement: UndefOr[Element]).orNull).collect {
            case textArea: HTMLTextAreaElement if (textArea.value: UndefOr[String]).isDefined => textArea.value
          }.filterNot(_.isEmpty)
           .foreach { value =>
            channel.pushNext(PasteEvent(value))
          }
        case _ =>
      }
    }

    val onKeyDown = (event: Event) => {
      val kbe = event.asInstanceOf[KeyboardEvent]
      val ctrl = kbe.ctrlKey
      val shift = kbe.shiftKey
      Option(
        kbe.polyfill() match {
          case (KCode.Right, None) => RightEvent(shift, ctrl)
          case (KCode.Left, None) => LeftEvent(shift, ctrl)
          case (KCode.Up, None) => UpEvent(shift)
          case (KCode.Down, None) => DownEvent(shift)
          case (KCode.End, None) => EndEvent(shift)
          case (KCode.Home, None) => HomeEvent(shift)
          case (KCode.PageDown, None) => PageDownEvent(shift)
          case (KCode.PageUp, None) => PageUpEvent(shift)
          case (KCode.Tab, None) => new TabEvent
          case (KCode.Delete, None) => new DeleteEvent
          case (KCode.Backspace, None) => new RemoveEvent
          case (KCode.Enter, _) => EnterEvent()

          case (KCode.Z, _) if shift && ctrl => RedoEvent
          case (KCode.Z, _) if ctrl => UndoEvent
          case (KCode.Y, _) if ctrl => RedoEvent

          case (KCode.V, _) if ctrl =>
            Option((document.activeElement: UndefOr[Element]).orNull).foreach {
              case textArea: HTMLTextAreaElement => textArea.value = ""
            }
            null

          case (KCode.A, _) if ctrl => SelectAll
          case (KCode.C, _) if ctrl => null // copy
          case (KCode.R, _) if ctrl => null // refresh
          case (KCode.W, _) if ctrl => null // close
          case (KCode.I, _) if shift & ctrl => null // dev tools

          case (keyCode, Some(charCode)) => CharEvent(keyCode, charCode.toChar)

          case notImplemented => null // anything else doesn't affect us (single shift/ctrl/alt press, escape, capslock, f3,f4, etc.)
        }
      ).foreach { e =>
        event.preventDefault()
        channel.pushNext(e)
      }
    }
    nav.focusTextArea()
    window.getSelection().removeAllRanges()
    nav.textArea.addEventListener("keyup", onKeyUp, useCapture = true)
    nav.textArea.addEventListener("keydown", onKeyDown, useCapture = true)
    ee.ondblclick = onDoubleClick
    ee.onclick = onClick
  }

  def onComplete(): Unit = ()
  def onNextSafe(elem: RxEvent): Future[Ack] = elem match {
    case e: NavbarEvent =>
      processNavbarEvent(e)
    case e: EditorEvent =>
      val sel = window.getSelection()
      val range = if (sel.rangeCount > 0 && sel.getRangeAt(0).toString != "") Option(sel.getRangeAt(0)) else None
      (e, range) match {
        case (se: SideEffect, Some(r)) =>
          val sch = r.startOffset
          val ech = r.endOffset
          val (sl, _) = r.startContainer.lineTextNodeByIndex()
          val (el, _) = r.endContainer.lineTextNodeByIndex()
          val replaceEvent = ReplaceEvent(Coord(sl, sch), Coord(el, ech), r.toString, se.keyCode)
          se match {
            case skip@(_: RemoveEvent | _: DeleteEvent) =>
              processEditorEvent(replaceEvent)
            case following =>
              processEditorEvent(replaceEvent).flatMap {
                case continue => processEditorEvent(following)
              }
          }
        case (userEvent, _) =>
          processEditorEvent(e)
      }
    case DocLoaded => Continue
  }

  private def reloadChanges(dataObs: Observable[(Int,Change)]): Future[Ack] = {
    val promise = Promise[Ack]()
    val future = promise.future
    nav.loadingDocument = true
    dataObs.doOnComplete {
      promise.completeWith(Continue)
      channel.pushNext(DocLoaded)
      nav.loadingDocument = false
      nav.pointer match {
        case PointerPX(bottom, left, top) => nav.moveCursor(Some(bottom), Some(left), Some(top))
      }
    }.foreach {
      case (_, Change(op, Indices(lidx, chidx), _)) =>
        import meter._
        nav.moveTo(Pointer.topLeft.v(lidx).>(lines.getLineTextUpTo(lidx, chidx)))
        op.execute(nav)
    }
    future
  }

  private def processNavbarEvent(e: RxEvent): Future[Ack] = e match {
    case NewDoc(_, created, None) =>
      resetEditor()
      store.storeName = created.toString
      channel.pushNext(DocLoaded)
      Continue
    case NewDoc(_, created, Some(data)) =>
      resetEditor()
      store.storeName = created.toString
      reloadChanges(store.add(data))
    case OpenDoc(aDoc) =>
      resetEditor()
      store.storeName = aDoc.created.toString
      /* omit first "virtual" entry that is basically entering the editor */
      reloadChanges(store.get(store.allKeys(Direction.Next)))
    case SaveDoc => Continue
    case _ => Continue
  }

  private def processEditorEvent(e: RxEvent): Future[Ack] = {
    e match {
      case selectable: Selectable if selectable.shifted =>
      case clickEvent: ClickEvent =>
      case clickEvent: DoubleClickEvent =>
      case _ => window.getSelection().removeAllRanges()
    }

    e match {
      case SelectAll =>
        val lines = nav.lines
        if (lines.lift(0).fold(false)(_.firstElementChild.hasChildNodes())) {
          val sel = window.getSelection()
          val newRange = document.createRange()
          newRange.setStart(lines.head.firstElementChild.firstChild, 0)
          val lastTextNode = lines.last.firstElementChild.firstChild
          newRange.setEnd(lastTextNode, lastTextNode.textContent.toString.length)
          sel.addRange(newRange)
        }
        Continue

      case e: Selectable if e.shifted =>
        val sel = window.getSelection()
        val r = if (sel.rangeCount == 0) document.createRange() else sel.getRangeAt(0)
        val optRange = e.move(nav, Option(r))
        sel.removeAllRanges()
        optRange.foreach {
          finalRange => sel.addRange(finalRange)
        }
        Continue

      case e: DoubleClickEvent =>
        nav.focusTextArea()
        e.move(nav, None)
        Continue

      case e: ClickEvent =>
        val sel = window.getSelection()
        val tempRange = if (sel.rangeCount == 0) None else Option(sel.getRangeAt(0))
        nav.focusTextArea()
        val current = window.getSelection()
        current.removeAllRanges()
        tempRange.collect {
          case r: Range if r.toString != "" =>
            current.addRange(r)
        }
        e.move(nav, None)
        Continue

      case e: Selectable =>
        e.move(nav, None)
        Continue

      case UndoEvent =>
        revert().onCompleteNewTx { lastCol =>
          lastCol.headOption match { // pixel coordinates are not stored in DB, they must be recalculated on UNDO
            case Some((_, Change(op, _, Indices(l, char)))) =>
              import meter._
              nav.moveTo(Pointer.topLeft.v(l).>(lines.getLineTextUpTo(l, char)))
              stash(op.revert(nav))
              Continue
            case None => Continue
          }
        }.asCompletedFuture

      case RedoEvent =>
        if (stashSize > 0) {
          val Change(op, _, Indices(lidx, chidx)) = unStash()
          import meter._
          nav.moveTo(Pointer.topLeft.v(lidx).>(lines.getLineTextUpTo(lidx, chidx)))
          val redone = op.execute(nav)
          stashApply(redone).asCompletedFuture
        } else {
          Continue
        }

      case e: SideEffect =>
        Future(e.mutate(nav)).flatMap {
          case Some(change) => commit(change).asCompletedFuture
          case None => Continue
        }

      case _ => Continue
    }
  }

}

object EditorComponent {
  def apply(channel: SubjectChannel[RxEvent, RxEvent], idb: IndexedDb)(implicit s: Scheduler) = new EditorComponent(channel, idb)
}