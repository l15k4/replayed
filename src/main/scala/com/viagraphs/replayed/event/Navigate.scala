package com.viagraphs.replayed.event

import com.viagraphs.replayed._
import com.viagraphs.scalajs.dom.KCode
import org.scalajs.dom.Range

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

case class LeftEvent(shifted: Boolean, quickly: Boolean) extends EditorEvent with Selectable {
  val keyCode = KCode.Left
  def move(nav: Navigator, optRange: Option[Range]): Option[Range] = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav
    import meter._
    if (ch == 0) {
      if (l != 0) {
        val prevLineTextNodeOpt = lines(l-1).firstElementChild.firstChild
        val toP = Option(prevLineTextNodeOpt).fold(p.^()) { prevLineTextNode =>
          p.^().>(prevLineTextNode.textContent)
        }
        nav.moveVertically(toP)
      }
    } else {
      val width: Array[Double] = if (quickly) lines.getTextUntilSpaceOrStart(l,ch) else lines.getLineChar(l, ch - 1)
      nav.moveHorizontally(p.<(width))
    }
    optRange.flatMap{ r =>
      select(r, p, nav.pointer, lines)
    }
  }
}

case class RightEvent(shifted: Boolean, quickly: Boolean) extends EditorEvent with Selectable {
  val keyCode = KCode.Right
  def move(nav: Navigator, optRange: Option[Range]): Option[Range] = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav
    import meter._
    val text = lines(l).firstElementChild.textContent
    val nextTextNode = lines.line(l+1).map(_.firstElementChild.firstChild)
    if (ch < text.length) {
      val width: Array[Double] = if (quickly) lines.getTextUntilSpaceOrEnd(l,ch) else text.charAt(ch)
      nav.moveHorizontally(p.>(width))
    } else {
      nextTextNode.fold(p)(nextLine => nav.moveVertically(p.<<.v()))
    }
    optRange.flatMap{ r =>
      select(r, p, nav.pointer, lines)
    }
  }
}

case class UpEvent(shifted: Boolean = false) extends EditorEvent with Selectable {
  val keyCode = KCode.Up
  def move(nav: Navigator, optRange: Option[Range]): Option[Range] = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav
    import meter._
    if (l > 0) {
      nav.moveVertically(p.<<.^().>(lines.getLineTextUpTo(l-1,ch)))
    } else {
      nav.moveHorizontally(p.<<)
    }
    optRange.flatMap{ r =>
      select(r, p, nav.pointer, lines)
    }
  }
}

case class DownEvent(shifted: Boolean = false) extends EditorEvent with Selectable {
  val keyCode = KCode.Down
  def move(nav: Navigator, optRange: Option[Range]): Option[Range] = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav
    import meter._
    val lineTextNode = lines(l).firstElementChild.firstChild
    val nextLine = lines.line(l + 1)
    if (nextLine.isDefined) {
      val nextLineSpanTextPart = lines.getLineTextUpTo(l+1,ch)
      nav.moveVertically(p.<<.v().>(nextLineSpanTextPart))
    } else {
      nav.moveHorizontally(p.>(lineTextNode.textContent.substring(ch)))
    }
    optRange.flatMap{ r =>
      select(r, p, nav.pointer, lines)
    }
  }
}

case class EndEvent(shifted: Boolean = false) extends EditorEvent with Selectable {
  val keyCode = KCode.Down
  def move(nav: Navigator, optRange: Option[Range]): Option[Range] = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav
    import meter._
    val line = lines(l).firstElementChild
    val text = line.textContent
    val rest = text.substring(ch, text.length).toCharArray
      if (rest.length > 0) {
        nav.moveHorizontally(p.>(rest))
        optRange.flatMap{ r =>
          select(r, p, nav.pointer, lines)
        }
      } else {
        None
      }
  }

}

case class HomeEvent(shifted: Boolean = false) extends EditorEvent with Selectable {
  val keyCode = KCode.Down
  def move(nav: Navigator, optRange: Option[Range]): Option[Range] = {
    val Navigator(lines, _, p) = nav
    if (nav.pointer.chidx > 0) {
      nav.moveHorizontally(nav.pointer.<<)
      optRange.flatMap { r =>
        select(r, p, nav.pointer, lines)
      }
    } else {
      None
    }
  }

}

case class PageUpEvent(shifted: Boolean = false) extends EditorEvent with Selectable {
  val keyCode = KCode.PageUp
  def move(nav: Navigator, optRange: Option[Range]): Option[Range] = {
    val Navigator(lines, _, p@Indices(l, _)) = nav
    if (l > 0) {
      nav.moveVertically(Pointer.topLeft)
    } else {
      nav.moveHorizontally(p.<<)
    }
    optRange.flatMap { r =>
      select(r, p, nav.pointer, lines)
    }
  }

}

case class PageDownEvent(shifted: Boolean = false) extends EditorEvent with Selectable {
  val keyCode = KCode.PageDown
  def move(nav: Navigator, optRange: Option[Range]): Option[Range] = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav
    import meter._
    val lineTextNode = lines(l).firstElementChild.firstChild
    val nextLine = lines.line(l + 1)
    if (nextLine.isDefined) {
      nav.moveVertically(p.<<.v(lines.length-1-l).>(lines(lines.length-1).firstElementChild.textContent))
    } else {
      nav.moveHorizontally(p.>(lineTextNode.textContent.substring(ch)))
    }
    optRange.flatMap { r =>
      select(r, p, nav.pointer, lines)
    }
  }

}

case class ClickEvent(lidxOpt: Option[Int], relativeX: Double, shifted: Boolean = false) extends EditorEvent with Selectable {
  val keyCode = 0
  def move(nav: Navigator, optRange: Option[Range]): Option[Range] = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav
    @tailrec def sumUntil(wit: Iterator[Double], sum: Double = 0D, acc: ListBuffer[Double] = ListBuffer()): ListBuffer[Double] = {
      if (sum < relativeX && wit.hasNext) {
        val w = wit.next()
        if (sum + w/2 < relativeX) {
          sumUntil(wit, sum + w, acc += w)
        } else {
          acc
        }
      } else {
        acc
      }
    }
    def moveCursor(lidx: Int): Unit = {
      val widths = sumUntil(meter.memoizedMeasure(lines.getLineText(lidx).toCharArray).iterator).toArray
      val lineDiff = lidx - l
      val newP =
        if (lineDiff > 0) {
          p.<<.v(lineDiff).>(widths)
        } else if (lineDiff == 0) {
          p.<<.>(widths)
        } else {
          p.<<.^(-lineDiff).>(widths)
        }
      nav.moveVertically(newP)
    }

    lidxOpt match {
      case Some(lidx) =>
        moveCursor(lidx)
      case None => // click happened out of line range
        moveCursor(lines.length - 1)
    }
    optRange.flatMap { r =>
      select(r, p, nav.pointer, lines)
    }
  }

}

case class DoubleClickEvent(lidx: Int, leftWidth: Double) extends EditorEvent {
  val keyCode = 0
  def move(nav: Navigator, optRange: Option[Range]): Unit = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav

    @tailrec def sumUntil(wit: Iterator[(Char, Double)], sum: Double = 0D, acc: ListBuffer[Double] = ListBuffer()): ListBuffer[Double] = {
      if (wit.hasNext) {
        val (char,width) = wit.next()
        if (sum < leftWidth || char != ' ') {
          sumUntil(wit, sum + width, acc += width)
        } else {
          acc
        }
      } else {
        acc
      }
    }
    val chars = lines.getLineText(l).toCharArray
    val widths = sumUntil(chars.zip(meter.memoizedMeasure(chars)).iterator).toArray
    val lineDiff = lidx - l

    val newP =
      if (lineDiff > 0) {
        p.<<.v(lineDiff).>(widths)
      } else if (lineDiff == 0) {
        p.<<.>(widths)
      } else {
        p.<<.^(lineDiff).>(widths)
      }

    nav.moveVertically(newP)
  }
}