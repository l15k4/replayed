package com.viagraphs.replayed

import monifu.concurrent.Scheduler
import monifu.reactive.Observable
import org.scalajs.dom.html.{Div, TextArea}
import org.scalajs.dom.document
import upickle.key

class Navigator(val pe: Div, val lines: Lines, val meter: Meter, var pointer: Pointer = Pointer.topLeft)(implicit s: Scheduler) {

  /* this state is a temporary solution to a performance problem */
  var cursorLocked = false
  var loadingDocument = false

  val ee = pe.parentElement

  lazy val cursor = document.createElement("div").markup[Div] { cursor =>
    cursor.className = "e_cursor"
    cursor.style.height = Lines.LineHeight + "px"
    cursor.style.width = Meter.charWidths.head._2 + "px"

    import scala.concurrent.duration._
    Observable.interval(500.millis).foreach { tick =>
      if (!cursorLocked) {
        if (tick % 2 == 0)
          cursor.style.visibility = "visible"
        else
          cursor.style.visibility = "hidden"
      }
    }
    pe.appendChild(cursor)
  }

  lazy val textArea = document.createElement("textarea").markup[TextArea] { textArea =>
    textArea.className = "e_textarea"
    textArea.wrap = "off"
    textArea.spellcheck = false
    textArea.style.opacity = "0"
    textArea.style.height = Lines.LineHeight + "px"
    textArea.style.width = Meter.charWidths.head._2 + "px"
    pe.appendChild(textArea)
  }

  def moveCursor(bottomOpt: Option[Double], leftOpt: Option[Double], topOpt: Option[Double]) = {
    import scala.concurrent.duration._
    if (!loadingDocument) {
      cursorLocked = true
      cursor.style.visibility = "visible"

      bottomOpt.foreach { bottom =>
        pe.style.bottom = bottom + "px"
      }

      topOpt.foreach { top =>
        pe.style.top = top + "px"
      }

      leftOpt.foreach { left =>
        pe.style.left = left + "px"
      }

      s.scheduleOnce( 500.millis, {
          cursorLocked = false
        }
      )
    }
  }

  def reset(): Unit = {
    moveTo(Pointer.topLeft)
    focusTextArea()
  }

  def focusTextArea() = {
    textArea.focus()
  }

  def eol = lines.getLineText(pointer.lidx).length == pointer.chidx
  def bol = pointer.chidx == 0
  def eof = lines.line(pointer.lidx+1).isEmpty
  def bof = lines.line(pointer.lidx-1).isEmpty

  def moveTo(p: Pointer): Pointer = {
    if (pointer.bottom != p.bottom)
      moveVertically(p)
    else
      moveHorizontally(p)
  }

  def moveVertically(p: Pointer): Pointer = p match { case PointerPX(bottom, left, top) =>
    moveCursor(Some(bottom), Some(left), Some(top))
    // TODO horizontal scrolling
    val scrollLeft = ee.scrollLeft
    val scrollRight = scrollLeft + Lines.LinesRight

    if (left + 30 > scrollRight) {
      ee.scrollLeft = (left - Lines.LinesRight + 20).ceil.toInt
    } else if (left < scrollLeft) {
      ee.scrollLeft = left.ceil.toInt - 30
    }
    pointer = p
    pointer
  }

  def moveHorizontally(p: Pointer): Pointer = p match { case PointerPX(bottom, left, top) =>
    moveCursor(None, Some(left), None)

    val scrollLeft = ee.scrollLeft
    val scrollRight = scrollLeft + Lines.LinesRight

    if (left + 30 > scrollRight) {
      ee.scrollLeft = (left - Lines.LinesRight + 20).ceil.toInt
    } else if (left < scrollLeft) {
      ee.scrollLeft = left.ceil.toInt - 30
    }
    pointer = p
    pointer
  }
}

object Navigator {
  def unapply(nav: Navigator): Option[(Lines, Meter, Pointer)] = Option((nav.lines,nav.meter,nav.pointer))
}

trait Arrows {
  def ^(times: Int = 1): Pointer
  def v(times: Int = 1): Pointer
  def <(widths: Array[Double]): Pointer
  def >(widths: Array[Double]): Pointer
}

trait Indices {

  def lidx: Int
  def chidx: Int

  def >(i: Indices) =
    if (lidx == i.lidx)
      chidx > i.chidx
    else
      lidx > i.lidx

  def <(i: Indices) =
    if (lidx == i.lidx)
      chidx < i.chidx
    else
      lidx < i.lidx
}

case class Coord(@key("y") lidx: Int, @key("x") chidx: Int) extends Indices

case class Pointer(lidx: Int, chidx: Int, bottom: Double, left: Double) extends Indices with Arrows {
  def << : Pointer = this.copy(lidx, chidx=0, bottom, Lines.LinesLeft)

  def ^(times: Int = 1): Pointer = {
    require(lidx-times >= 0, (lidx-times) + " is not a valid p.x value to move up to")
    val newBottom = bottom - (Lines.LineHeight * times)
    require(newBottom >= Lines.LineHeight, newBottom + " reaches over the top of lines element !")
    this.copy(lidx-times, chidx, newBottom, left)
  }

  def v(times: Int = 1): Pointer = {
    val newBottom = bottom + (Lines.LineHeight * times)
    this.copy(lidx+times,chidx, newBottom, left)
  }

  def <(widths: Array[Double]): Pointer = {
    val times = widths.length
    require(chidx-times >= 0, (chidx-times) + " is not a valid p.y value to move left to")
    val newLeft = left - widths.sum
    require(newLeft >= Lines.LinesLeft, newLeft + " reaches over the left boundary of lines element !")
    this.copy(lidx,chidx-times, bottom, newLeft)
  }

  def >(widths: Array[Double]): Pointer = {
    val newRight = left + widths.sum
    // require(newRight <= Lines.LinesRight, newRight + " reaches over the right boundary of lines element !") TODO feature line soft wrap
    this.copy(lidx,chidx + widths.length, bottom, newRight)
  }
}

object Indices {
  def unapply(p: Indices): Option[(Int, Int)] = Some((p.lidx, p.chidx))
}
object PointerPX {
  def unapply(p: Pointer): Option[(Double, Double, Double)] = Some((p.bottom, p.left, p.bottom - Lines.LineHeight))
}
object Pointer {
  def topLeft = Pointer(0,0, Lines.LineHeight, Lines.LinesLeft)
}
