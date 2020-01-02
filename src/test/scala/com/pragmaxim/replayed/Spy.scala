package com.pragmaxim.replayed

import com.pragmaxim.replayed.event._
import com.pragmaxim.replayed.mvc.{EditorComponent, StashingChangeLog}
import monifu.reactive.Observable

import scala.collection.mutable.ListBuffer
import scala.util.Random

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