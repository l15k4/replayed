package com.pragmaxim.replayed.event

import com.pragmaxim.replayed._
import org.scalajs.dom.{Element, Range}

case object SelectAll extends EditorEvent

sealed trait Rangy {
  def range: Range
  def lines: Lines
  def from: Indices
  def to: Indices

  lazy val Indices(fl, fch) = from
  lazy val Indices(tl, tch) = to

  lazy val toNode = Option(lines(tl).firstElementChild).map {
    case span: Element if span.hasChildNodes() => span.firstChild
    case span => span
  }.get

  lazy val fromNode = Option(lines(fl).firstElementChild).map {
    case span: Element if span.hasChildNodes() => span.firstChild
    case span => span
  }.get

  lazy val stuck = fl == tl && fch == tch
}

case class NewRangy(range: Range, from: Indices, to: Indices, lines: Lines) extends Rangy {
  def newRange() = {
    if (fl == tl) {
      range.setStart(toNode, Math.min(tch, fch))
      range.setEnd(toNode, Math.max(tch, fch))
    } else {
      if (tl > fl) {
        range.setStart(fromNode, fch)
        range.setEnd(toNode, tch)
      } else {
        range.setStart(toNode, tch)
        range.setEnd(fromNode, fch)
      }
    }
  }
}

case class OldRangy(range: Range, from: Indices, to: Indices, lines: Lines) extends Rangy {
  lazy val sch = range.startOffset
  lazy val ech = range.endOffset
  lazy val (sl, sCnt) = range.startContainer.lineTextNodeByIndex()
  lazy val (el, eCnt) = range.endContainer.lineTextNodeByIndex()

  lazy val start = Coord(sl, sch)
  lazy val end = Coord(el, ech)

  lazy val fromStart = fl == sl && fch == sch
  lazy val toStart = tl == sl && tch == sch
  lazy val toEnd = tl == el && tch == ech
  lazy val fromEnd = fl == el && fch == ech

  val conditionalActions = Map[String,(() => Boolean,() => Unit)](
    ("exits", (
          () => (fromStart && toEnd) || (fromEnd && toStart),
          () => if (fromStart) range.collapse(false) else range.collapse(true)
      )
    ),
    ("expandsFromStart", (
          () => fromStart && start > to && to < end,
          () => range.setStart(toNode, tch)
      )
    ),
    ("expandsFromEnd", (
          () => fromEnd && start < to && to > end,
          () => range.setEnd(toNode, tch)
      )
    ),
    ("shrinksFromStart", (
          () => fromStart && start < to && to < end,
          () => range.setStart(toNode, tch)
      )
    ),
    ("shrinksFromEnd", (
          () => fromEnd && start < to && to < end,
          () => range.setEnd(toNode, tch)
      )
    ),
    ("switchFromStart", (
          () => fromStart && start < to && to > end,
          () => {
            range.setStart(eCnt, ech)
            range.setEnd(toNode, tch)
          }
      )
    ),
    ("switchFromEnd", (
          () => fromEnd && start > to && to < end,
          () => {
            range.setEnd(sCnt, sch)
            range.setStart(toNode, tch)
          }
      )
    )
  )
}

trait Selectable {
  def shifted: Boolean
  def move(nav: Navigator, range: Option[Range]): Option[Range]
  def select(r: Range, fc: Indices, tc: Indices, lines: Lines): Option[Range] = {
   val rangy =
    if (r.toString == "")
      NewRangy(r, fc, tc, lines)
    else
      OldRangy(r, fc, tc, lines)
    receive(rangy)
  }
  def receive: PartialFunction[Rangy, Option[Range]] = {
    case r: NewRangy if r.stuck => None
    case r: OldRangy if r.stuck => Option(r.range)

    case r: NewRangy =>
      r.newRange()
      Option(r.range)
    case r: OldRangy =>
      val passed = r.conditionalActions.filter {
        case (name, ifThen) => ifThen._1()
      }
      if (passed.size > 1) {
        throw new IllegalStateException(passed.fold("Multiple matches : ") { case (acc, (name, _)) => acc + " " + name}.toString)
      } else if (passed.size == 0) {
        throw new IllegalStateException(s"No match for rangy from ${r.from} to ${r.to}, $r")
      }
      passed.head match {
        case (name, (cond, action)) if name == "exits" =>
          action()
          None
        case (name, (cond, action)) =>
          action()
          require(r.range.toString != "", s"Invalid selection $name")
          Option(r.range)
      }
    case x => throw new MatchError("Unable to match : " + x)
  }
}