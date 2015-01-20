package com.viagraphs.replayed.event

import com.viagraphs.replayed._
import com.viagraphs.scalajs.dom.KCode
import scala.collection.immutable.TreeMap

trait SideEffect {
  def keyCode: Int
  def mutate(nav: Navigator): Option[Change]
}

class DeleteEvent extends EditorEvent with SideEffect {
  val keyCode = KCode.Delete
  override def toString = getClass.getSimpleName
  def mutate(nav: Navigator) = {
    if (nav.eol) // when at EOL
      if (nav.eof) // and EOF
        None // do nothing
      else // remove next line and concatenate it to current one
        Option(EolDelete.execute(nav))
    else // just delete string
      Option(MolDelete().execute(nav))
  }
}

case class EnterEvent(indent: Boolean = true) extends EditorEvent with SideEffect {
  val keyCode = KCode.Enter
  val char = keyCode

  def mutate(nav: Navigator) = {
    val Navigator(lines, _, Indices(l,ch)) = nav
    Option(
      if (lines.getLineText(l).length == ch) // when EOL
        EolEnter(indent).execute(nav)
      else
        MolEnter(indent).execute(nav)
    )
  }
}

case class CharEvent(keyCode: Int, char: Char) extends EditorEvent with SideEffect {
  def mutate(nav: Navigator) = Option(CharInput(char).execute(nav))
}

class TabEvent extends EditorEvent with SideEffect {
  val strRepr = "    " //markdown tab is 4 spaced
  val keyCode = KCode.Tab
  def mutate(nav: Navigator) = Option(Insert(strRepr).execute(nav))
  override def toString = getClass.getSimpleName
}

class RemoveEvent extends EditorEvent with SideEffect {
  val keyCode = KCode.Backspace
  override def toString = getClass.getSimpleName
  def mutate(nav: Navigator) = {
    if (nav.bol) // when at BOL
      if (nav.bof) // when at BOF too, do nothing
        None // do nothing
      else // otherwise remove current line and concatenate it to prev one
        Option(BolRemove.execute(nav))
    else // just remove string
      Option(MolRemove().execute(nav))
  }
}

case class PasteEvent(text: String) extends EditorEvent with SideEffect {
  def keyCode: Int = KCode.V
  def mutate(nav: Navigator) = {
    val inputLines = text.replaceAll("""<(?!\/?a(?=>|\s.*>))\/?.*?>""", "").split("\n") //TODO convert tabs to spaces
    if (inputLines.length == 1) {
      Option(Insert(inputLines.head).execute(nav))
    } else {
      val Navigator(lines, _, p@Indices(l,ch)) = nav
      val current = lines(l).firstElementChild.textContent

      val linesBetween = inputLines.drop(1).dropRight(1)
      val start = (l, (current.substring(0,ch), inputLines.head))

      val it = linesBetween.iterator
      val toPaste =
        for(
          i <- l+1 to l+linesBetween.length if it.hasNext
        ) yield (i, ("", it.next()))

      val end = (l+inputLines.length-1, (current.substring(ch), inputLines.last))

      val result = start :: (toPaste.toList :+ end)
      Option(Paste(TreeMap(result:_*), false).execute(nav))
    }

  }
}

case class ReplaceEvent(fp: Indices, tp: Indices, text: String, keyCode: Int) extends EditorEvent with SideEffect {
  def mutate(nav: Navigator) = {
    val Indices(sl, sch) = fp
    val Indices(el ,ech) = tp
    val Indices(lidx, chidx) = nav.pointer
    val starts = sl == lidx && sch == chidx

    if (sl == el) {
      if (starts) Option(MolDelete(Option(text)).execute(nav)) else Option(MolRemove(Option(text)).execute(nav))
    } else {
      val lines = nav.lines
      val head = lines(sl).firstElementChild.textContent
      val start = (sl, (head.substring(0,sch), head.substring(sch)))

      val toRemove =
        for(
          i <- sl+1 until el
        ) yield (i, ("", lines(i).firstElementChild.textContent))

      val last = lines(el).firstElementChild.textContent
      val end = (el, (last.substring(ech), last.substring(0,ech)))

      val result = start :: (toRemove.toList :+ end)
      Option(Replace(TreeMap(result:_*), starts).execute(nav))
    }
  }
}