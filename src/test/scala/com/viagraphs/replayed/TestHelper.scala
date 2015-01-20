package com.viagraphs.replayed

import com.viagraphs.idb._
import com.viagraphs.scalajs.dom.KCode
import com.viagraphs.replayed.event._
import com.viagraphs.replayed.mvc.{ReplayDispatcher, EditorComponent, StashingChangeLog}
import monifu.reactive.Ack.Continue
import monifu.reactive.{Ack, Observable}
import monifu.reactive.channels.SubjectChannel
import org.scalajs.dom.{HTMLDivElement, document}
import utest._

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.Dynamic.{literal => lit}
import scala.util.Random
import scala.util.control.NonFatal

trait EditorHelper extends Spy {
  import com.viagraphs.idb.IdbSupport._

import scala.language.implicitConversions
  implicit val scheduler = IndexedDb.scheduler
  val created = new js.Date().getTime()
  val indexedDb = // TODO should be closed after test suite run, as it used to when using uTest programmatically
    IndexedDb(
      new RecreateDb(s"editorSuite",
        Some { (db, ve) =>
          db.createObjectStore(created.toString, lit("autoIncrement" -> true))
          ()
        }
      ) with Profiling)
  val docStore = indexedDb.openStore[Int,Change](created.toString)

  def pb(lidx: Int) = (lidx + 1) * Lines.LineHeight + Lines.LinesTop

  def pl(widths: Array[Double]) = Lines.LinesLeft + widths.sum

  def pos(lidx: Int, str: String)(implicit m: Meter) = Pointer(lidx, str.length, pb(lidx), pl(m.memoizedMeasure(str.toCharArray)))

  case class Spec(inputStr: String, texts: Seq[String], position: (Int, String), r: Option[String])
  def prepare(inputStr: String, texts: Seq[String], position: (Int, String), r: Option[String]): Future[(String, Seq[String], String)] = {
    import org.scalajs.dom.extensions.PimpedHtmlCollection
    document.getSelection().removeAllRanges()
    if (document.body.hasChildNodes())
      document.body.children.foreach(document.body.removeChild(_))

    document.body.appendChild(
      document.createElement("div").markup[HTMLDivElement] { lines =>
        lines.id = "e_editor"
        lines.className = "e_editor"
      }
    )
    docStore.clear.onCompleteNewTx { whatever =>
      ReplayDispatcher{ channel =>
        List(new MockEditorComponent(Spec(inputStr, texts, position, r), channel, indexedDb))
      }
    }.asCompletedFuture.map(e => (inputStr, texts, r.fold("No Range"){range => document.getSelection().getRangeAt(0).toString}))
  }

  class MockEditorComponent(spec: Spec, channel: SubjectChannel[RxEvent, RxEvent], idb: IndexedDb) extends EditorComponent(channel, idb) with LogSpy {
    val clickRegex = "\\[(\\d+)?,(\\d+\\.\\d+)\\]".r
    val pasteRegex = """\[([\s\S]*)\]""".r

    case object AssertEvent extends RxEvent {
      def assertState() = {
        implicit val meter = nav.meter
        val Spec(_, texts, position, r) = spec
        try {
          r match {
            case Some(range) =>
              val actualRange = document.getSelection().getRangeAt(0).toString
              assert(actualRange == range)
            case None =>
              val actualRangeCount = document.getSelection().rangeCount
              assert(actualRangeCount == 0)
          }

          val actualLineLength = lines.length
          val expectedLineLength = texts.size
          assert(actualLineLength == expectedLineLength)
          for (index <- 0 until texts.size) {
            val actualLineText = lines.getLineText(index)
            val expectedLineText = texts(index)
            assert(actualLineText == expectedLineText)
          }
          val expectedPosition = pos(position._1, position._2)
          assert(nav.pointer == expectedPosition)
          channel.pushComplete()
        } catch {
          case NonFatal(ex) => channel.pushError(ex)
        }
      }
    }

    override def onError(ex: Throwable): Unit = {
      channel.pushError(ex)
    }

    override def onNextSafe(elem: RxEvent): Future[Ack] = {
      elem match {
        case AssertEvent =>
          AssertEvent.assertState()
          Continue
        case _ => super.onNextSafe(elem)
      }
    }

    override def onSubscribe() = {
      val input = spec.inputStr
      val events = input.substring(1, input.length).split('|').map { symbol =>
        MockEditorComponent.events.get(symbol) match {
          case Some(x) => x
          case _ => symbol match {
            case clickRegex(null, pageX) => new ClickEvent(None, pageX.toDouble - Lines.LinesLeft) with SelectableSpy
            case clickRegex(lidx, pageX) => new ClickEvent(Some(lidx.toInt), pageX.toDouble - Lines.LinesLeft) with SelectableSpy
            case pasteRegex(text) => new PasteEvent(text) with SideEffectSpy
          }
        }
      }
      channel.pushNext(NewDoc(created = created, data = None))
      Observable.fromIterable(events)
        .doOnComplete{
          channel.pushNext(AssertEvent)
        }.foreach(e => channel.pushNext(e))(scheduler)
    }

    /* POSSIBLE PROFILING
    override def onNextSafe(elem: RxEvent): Future[Ack] = {
      val start = new js.Date().getTime()
      val name = elem.name
      super.onNextSafe(elem).map { x =>
        TestSuites.profilingMap.get(name) match {
          case Some(total) => TestSuites.profilingMap = TestSuites.profilingMap.updated(name, total + (new js.Date().getTime - start))
          case None => TestSuites.profilingMap = TestSuites.profilingMap.updated(name, new js.Date().getTime - start)
        }
        x
      }
    }
    */

  }

  object MockEditorComponent {
    val events = Map[String, RxEvent](
      Symbol.`<~` -> new LeftEvent(false, true) with SelectableSpy,
      Symbol.`~>` -> new RightEvent(false, true) with SelectableSpy,
      Symbol.`<_~` -> new LeftEvent(true, true) with SelectableSpy,
      Symbol.`~_>` -> new RightEvent(true, true) with SelectableSpy,
      Symbol.`<` -> new LeftEvent(false, false) with SelectableSpy,
      Symbol.`>` -> new RightEvent(false, false) with SelectableSpy,
      Symbol.`^` -> new UpEvent with SelectableSpy,
      Symbol.`v` -> new DownEvent with SelectableSpy,
      Symbol.`>>` -> new EndEvent with SelectableSpy,
      Symbol.`<<` -> new HomeEvent with SelectableSpy,
      Symbol.`^^` -> new PageUpEvent with SelectableSpy,
      Symbol.`vv` -> new PageDownEvent with SelectableSpy,
      Symbol.`_<` -> new LeftEvent(true, false) with SelectableSpy,
      Symbol.`_>` -> new RightEvent(true, false) with SelectableSpy,
      Symbol.`_^` -> new UpEvent(true) with SelectableSpy,
      Symbol.`_v` -> new DownEvent(true) with SelectableSpy,
      Symbol.`_>>` -> new EndEvent(true) with SelectableSpy,
      Symbol.`_<<` -> new HomeEvent(true) with SelectableSpy,
      Symbol.`_^^` -> new PageUpEvent(true) with SelectableSpy,
      Symbol.`_vv` -> new PageDownEvent(true) with SelectableSpy,
      Symbol.z -> UndoEvent,
      Symbol.y -> RedoEvent,
      Symbol.tab -> new TabEvent with SideEffectSpy,
      Symbol.del -> new DeleteEvent with SideEffectSpy,
      Symbol.rm -> new RemoveEvent with SideEffectSpy,
      Symbol.nl -> new EnterEvent with SideEffectSpy,
      Symbol.a -> new CharEvent(KCode.A, 'a') with SideEffectSpy,
      Symbol.b -> new CharEvent(KCode.B, 'b') with SideEffectSpy,
      Symbol.c -> new CharEvent(KCode.C, 'c') with SideEffectSpy,
      Symbol.`1` -> new CharEvent(KCode.Num1, '1') with SideEffectSpy,
      Symbol.`2` -> new CharEvent(KCode.Num2, '2') with SideEffectSpy,
      Symbol.`3` -> new CharEvent(KCode.Num3, '3') with SideEffectSpy,
      Symbol.`4` -> new CharEvent(KCode.Num4, '4') with SideEffectSpy,
      Symbol.`5` -> new CharEvent(KCode.Num5, '5') with SideEffectSpy,
      Symbol.`6` -> new CharEvent(KCode.Num6, '6') with SideEffectSpy,
      Symbol.`7` -> new CharEvent(KCode.Num7, '7') with SideEffectSpy,
      Symbol.`8` -> new CharEvent(KCode.Num8, '8') with SideEffectSpy,
      Symbol.`9` -> new CharEvent(KCode.Num9, '9') with SideEffectSpy,
      Symbol.` ` -> new CharEvent(KCode.Space, ' ') with SideEffectSpy
    )
  }

}

trait Spy {

  object Symbol {
    val `<` = "<"
    val `>` = ">"
    val `^` = "^"
    val `v` = "v"
    val `>>` = ">>"
    val `<<` = "<<"
    val `^^` = "^^"
    val `vv` = "vv"

    val `_<` = "_<"
    val `_>` = "_>"
    val `_^` = "_^"
    val `_v` = "_v"
    val `_>>` = "_>>"
    val `_<<` = "_<<"
    val `_^^` = "_^^"
    val _vv = "_vv"

    val `<~` = "<~"
    val `~>` = "~>"

    val `<_~` = "<_~"
    val `~_>` = "~_>"

    val z = "z"
    val y = "y"

    val tab = "tab"
    val del = "del"
    val rm = "rm"
    val nl = "nl"

    val a = "a"
    val b = "b"
    val c = "c"
    val `1` = "1"
    val `2` = "2"
    val `3` = "3"
    val `4` = "4"
    val `5` = "5"
    val `6` = "6"
    val `7` = "7"
    val `8` = "8"
    val `9` = "9"
    val ` ` = " "
  }


  trait Logger {
    def log(prefix: String, fp: Indices, tp: Indices, content: String, color: String = Random.shuffle(List(Console.RED, Console.GREEN, Console.CYAN, Console.MAGENTA, Console.YELLOW, Console.BLUE)).head) = {
      val Indices(fl, fch) = fp
      val Indices(tl, tch) = tp
      //      print(color + s"[$fl,$fch]|$prefix$content|[$tl,$tch] " + Console.RESET)
    }
  }

  trait SideEffectSpy extends SideEffect with Logger {
    abstract override def mutate(nav: Navigator): Option[Change] = {
      val prefix = this match {
        case s: EnterEvent => "enter"
        case s: DeleteEvent => Symbol.del
        case s: RemoveEvent => Symbol.rm
        case s: CharEvent => String.valueOf(s.char)
        case s: TabEvent => Symbol.tab
        case s: PasteEvent => s"paste[${s.text}]"
      }
      val fp = nav.pointer
      val result = super.mutate(nav)
      log(prefix, fp, result.map(_.tp).getOrElse(fp), "")
      result
    }
  }

  trait LogSpy extends StashingChangeLog {
    this: EditorComponent =>

    private def log(op: String, entry: Change) = {
      val Change(op, fp, tp) = entry
      // println(Console.YELLOW + s"[${fp.lidx},${fp.chidx} >> ${tp.lidx},${tp.chidx}] $op" + Console.RESET)
    }

    abstract override def commit(entry: Change): Observable[(Int,Change)] = {
      log("commit", entry)
      super.commit(entry)
    }

    abstract override def revert(): Observable[(Int,Change)] = {
      super.revert().doWork(entry => log("revert", entry._2))
    }

    abstract override def stash(entry: Change): ListBuffer[Change] = {
      log("stash", entry)
      super.stash(entry)
    }

    abstract override def unStash(): Change = {
      val entry = super.unStash()
      log("unStash", entry)
      entry
    }

    abstract override def stashApply(entry: Change): Observable[(Int,Change)] = {
      log("stashApply", entry)
      super.stashApply(entry)
    }
  }

  trait SelectableSpy extends Selectable with Logger {

    import org.scalajs.dom.Range

    val prefix = this match {
      case s: LeftEvent => if (shifted) Symbol.`_<` else Symbol.`<`
      case s: RightEvent => if (shifted) Symbol.`_>` else Symbol.`>`
      case s: UpEvent => if (shifted) Symbol.`_^` else Symbol.`^`
      case s: DownEvent => if (shifted) Symbol.`_v` else Symbol.`v`
      case s: EndEvent => if (shifted) Symbol.`_>>` else Symbol.`>>`
      case s: HomeEvent => if (shifted) Symbol.`_<<` else Symbol.`<<`
      case s: PageDownEvent => if (shifted) Symbol.`_vv` else Symbol.`vv`
      case s: PageUpEvent => if (shifted) Symbol.`_^^` else Symbol.`^^`
      case s: ClickEvent =>
        if (shifted) s"_click[${s.lidxOpt},${s.relativeX}]" else s"click[${s.lidxOpt},${s.relativeX}]"
    }

    abstract override def move(nav: Navigator, range: Option[Range]): Option[Range] = {
      val fp = nav.pointer
      val result = super.move(nav, range)
      val tp = nav.pointer
      if (!shifted)
        log(prefix, fp, tp, "")
      result
    }

    override def receive: PartialFunction[Rangy, Option[Range]] = {
      case rangy: Rangy =>
        val result = super.receive(rangy)
        log(prefix, rangy.from, rangy.to, result.fold("NONE")(_.toString))
        result
    }
  }

}