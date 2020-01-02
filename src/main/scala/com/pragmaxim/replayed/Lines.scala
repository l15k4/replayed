package com.pragmaxim.replayed

import org.scalajs.dom.html.{Div, Span}
import org.scalajs.dom.{Element, document}

import scala.collection.{AbstractSeq, IndexedSeq}
import scala.scalajs.js.UndefOr


class Lines(val le: Div) extends AbstractSeq[Element] with IndexedSeq[Element] {

  {
    initLines()
    val r = le.firstElementChild.getBoundingClientRect()
    Lines.LineHeight = r.height
    Lines.LinesLeft = r.left
    Lines.LinesRight = le.parentElement.getBoundingClientRect().right
  }

  def initLines(): Unit = {
    val initialLine = createLineDiv
    initialLine.appendChild(createLineSpan)
    le.appendChild(initialLine)
  }

  def reset(): Unit = {
    le.removeAllChildren()
    initLines()
  }

  def createLineDiv = document.createElement("div").markup[Div] { lineDiv =>
    lineDiv.className = "e_line"
    lineDiv.style.height = "12px"
  }
  def createLineSpan = document.createElement("span").markup[Div] {textSpan =>
    textSpan.className = "e_text"
    textSpan.style.pointerEvents = "none"
  }

  def length = le.childElementCount

  def line(idx: Int): Option[Element] = Option((apply(idx): UndefOr[Element]).orNull)

  def apply(idx: Int): Element = le.children(idx)

  def getLineText(idx: Int): String = {
    if (idx < 0 || idx >= length)
      throw new IndexOutOfBoundsException("Unable to get textContent from line : " + idx + " of children count " + length)
    apply(idx).firstElementChild.textContent
  }

  def getLineChar(lidx: Int, chidx: Int): Char = {
    val text = getLineText(lidx)
    require(chidx < text.length, s"Do not want me to give you char from index : $chidx in line text : $text")
    text.charAt(chidx)
  }

  def getTextUntilSpaceOrStart(lidx: Int, chidx: Int): String = {
    val text = getLineText(lidx)
    require(chidx <= text.length, s"Do not want me to give you char from index : $chidx in line text : $text")
    val leftStr = text.substring(0,chidx)
    val leftSpace = leftStr.lastIndexOf(' ')
    if (leftSpace == -1) {
      leftStr
    } else if (leftSpace == leftStr.length-1) {
      leftStr.reverse.takeWhile(_ == ' ').toString
    } else {
      leftStr.substring(leftSpace+1)
    }
  }

  def getTextUntilSpaceOrEnd(lidx: Int, chidx: Int): String = {
    val text = getLineText(lidx)
    require(chidx < text.length, s"Do not want me to give you char from index : $chidx in line text : $text")
    val rightStr = text.substring(chidx)
    val rightSpace = rightStr.indexOf(' ')
    if (rightSpace == -1) {
      rightStr
    } else if (rightSpace == 0) {
      rightStr.takeWhile(_ == ' ').toString
    } else {
      rightStr.substring(0, rightSpace)
    }
  }

  def getLineTextUpTo(lidx: Int, chidx: Int): Array[Char] = {
    getLineText(lidx) match {
      case text if chidx > text.length => text.toCharArray
      case text => text.substring(0, chidx).toCharArray
    }
  }

  def insertLine(idx: Int, text: String): Element = {
    if (idx < 0 || idx > length) throw new IndexOutOfBoundsException("Unable to insert element to : " + idx)
    val newLine = createLineDiv
    val span = createLineSpan.markup[Span](_.textContent = text)
    newLine.appendChild(span)
    le.insertBefore(newLine, apply(idx)).asInstanceOf[Element]
  }

  def removeLine(idx: Int): Element = {
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException("Unable to remove element at : " + idx)
    le.removeChild(apply(idx)).asInstanceOf[Element]
  }

  def updateLine(idx: Int, text: String): Element = {
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException("Unable to update node child at : " + idx + " with text :\n" + text)
    val child = apply(idx)
    val span = child.firstElementChild
    span.textContent = text
    child
  }

  def updateLine(idx: Int, fn: String => String): (String, Element, String) = {
    val before = apply(idx).firstElementChild.textContent
    val after = fn(before)
    (before, updateLine(idx, after), after)
  }

  def removeAndUpdatePrev(idx: Int, fn: String => String => String): (Element, Element) = {
    val removed = removeLine(idx)
    val updated = updateLine(idx-1, fn(removed.firstElementChild.textContent))._2
    (removed, updated)
  }

  override def toString(): String = {
    le.innerHTML
  }

}

object Lines {
  var LineHeight = 0D
  var LinesTop = 0D
  var LinesLeft = 0D
  var LinesRight = 0D
}