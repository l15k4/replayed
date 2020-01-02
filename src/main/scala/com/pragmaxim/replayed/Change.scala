package com.pragmaxim.replayed

import com.pragmaxim.idb.IdbSupport
import upickle.Js
import upickle.legacy._

import scala.collection.immutable.TreeMap

case class Change(op: Op, fp: Indices, tp: Indices)

sealed trait Op {

  def execute(nav: Navigator): Change

  def revert(nav: Navigator): Change

  protected def addString(str: String, pointer: Int)(l: String) = {
    if (pointer < 0 || pointer > l.length)
      throw new IndexOutOfBoundsException("Unable to add string to " + pointer + " in line " + l)
    else if (l.length == pointer)
      l + str
    else
      l.substring(0, pointer) + str + l.substring(pointer)
  }

  protected def rmChar(idx: Int)(l: String) = {
    if (idx < 0 || idx >= l.length) throw new IndexOutOfBoundsException("Unable to remove character from " + idx + " in line " + l)
    l.substring(0, idx) + l.substring(idx + 1)
  }

  protected def rmString(str: String, pointer: Int)(l: String) = {
    if (pointer < 0 || pointer > l.length) throw new IndexOutOfBoundsException("Unable to remove string from " + pointer + " in line " + l)
    l.substring(0, pointer - str.length) + l.substring(pointer)
  }

  protected def delString(str: String, pointer: Int)(l: String) = {
    if (pointer < 0 || pointer > l.length) throw new IndexOutOfBoundsException("Unable to delete string from " + pointer + " in line " + l)
    l.substring(0, pointer) + l.substring(pointer + str.length)
  }
}


/** DELETE */

@key("eolDel")
case object EolDelete extends Op {

  def execute(nav: Navigator) = {
    val Navigator(lines, _, p@Indices(l, ch)) = nav
    lines.removeAndUpdatePrev(l + 1, (r) => (u) => u + r)
    Change(this, p, p)
  }

  def revert(nav: Navigator) = {
    val Navigator(lines, _, p@Indices(l, ch)) = nav
    val line = lines.getLineText(l)
    (line.substring(0, ch), line.substring(ch)) match {
      case (remains, newLine) =>
        lines.updateLine(l, remains)
        lines.insertLine(l + 1, newLine)
        Change(this, p, p)
    }
  }
}

@key("molDel")
case class MolDelete(t: Option[String] = None) extends Op {

  def execute(nav: Navigator) = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav
    t match {
      case Some(target) =>
        lines.updateLine(l, delString(target, ch) _)
        Change(this, p, p)
      case None =>
        val deleted = lines.updateLine(l, rmChar(ch) _)._1.charAt(ch)
        Change(MolDelete(Option(Character.toString(deleted))), p, p)
    }
  }

  def revert(nav: Navigator) = {
    val Navigator(lines, _, p@Indices(l, ch)) = nav
    lines.updateLine(l, addString(t.get, ch) _)
    Change(this, p, p)
  }
}

/** ENTER */

@key("eolEtr")
case class EolEnter(i: Boolean) extends Op {

  protected def getIndent(l: String) = if (i) " " * l.takeWhile(_.isWhitespace).length else ""

  def execute(nav: Navigator) = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav
    import meter._
    val indentStr = getIndent(lines.getLineText(l))
    lines.insertLine(l + 1, indentStr)
    Change(this, p, nav.moveVertically(p.<<.v().>(indentStr)))
  }

  def revert(nav: Navigator) = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav
    import meter._
    lines.removeLine(l)
    Change(this, p, nav.moveVertically(p.<<.^().>(lines.getLineText(l - 1))))
  }
}

@key("molEtr")
case class MolEnter(i: Boolean) extends Op {

  protected def getIndent(l: String) = if (i) " " * l.takeWhile(_.isWhitespace).length else ""

  def execute(nav: Navigator) = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav
    val line = lines.getLineText(l)
    val indentStr = getIndent(line)
    (line.substring(0, ch), line.substring(ch)) match {
      case (remains, newLine) =>
        import meter._
        lines.updateLine(l, remains)
        lines.insertLine(l + 1, indentStr + newLine)
        Change(this, p, nav.moveVertically(p.<<.v().>(indentStr)))
    }
  }

  def revert(nav: Navigator) = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav
    import meter._
    var toBeUpdated = null.asInstanceOf[String]
    lines.removeAndUpdatePrev(l,
      (r) => (u) => {
        toBeUpdated = u
        val removed = if (i) r.dropWhile(_.isWhitespace) else r
        u + removed
      }
    )
    Change(this, p, nav.moveVertically(p.^().>(toBeUpdated)))
  }
}

/** REMOVE */

@key("bolRm")
case object BolRemove extends Op {

  def execute(nav: Navigator) = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav
    import meter._
    var toBeUpdated = null.asInstanceOf[String]
    lines.removeAndUpdatePrev(l, (r) => (u) => {
      toBeUpdated = u
      u + r
    }
    )
    Change(this, p, nav.moveVertically(p.^().>(toBeUpdated)))
  }

  def revert(nav: Navigator) = {
    val Navigator(lines, _, fp@Indices(l, ch)) = nav
    val tp =
      if (lines.getLineText(l).length == ch) // when EOL
        EolEnter(false).execute(nav).tp
      else
        MolEnter(false).execute(nav).tp
    Change(this, fp, tp)
  }
}

@key("molRm")
case class MolRemove(t: Option[String] = None) extends Op {

  def execute(nav: Navigator) = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav
    import meter._
    t match {
      case Some(target) =>
        val removedStr = lines.updateLine(l, rmString(target, ch) _)._1.substring(ch - target.length, ch)
        require(removedStr == target, s"We've hit an inconsistency, removed '$removedStr' but expected '$target' at $p")
        Change(this, p, nav.moveHorizontally(p.<(removedStr)))
      case None =>
        val removedChar = lines.updateLine(l, rmChar(ch - 1) _)._1.charAt(ch - 1)
        val removedString = Character.toString(removedChar)
        Change(MolRemove(Option(removedString)), p, nav.moveHorizontally(p.<(removedString)))
    }
  }

  def revert(nav: Navigator) = {
    t match {
      case Some(target) => Change(this, nav.pointer, Insert(target).execute(nav).tp)
      case None => throw new IllegalStateException("Unable to revert MolRemove, empty target")
    }
  }
}

/** ARBITRARY */

@key("ch")
case class CharInput(ch: Char) extends Op {

  def execute(nav: Navigator) = {
    val Navigator(lines, meter, p@Indices(lidx, chidx)) = nav
    import meter._
    lines.updateLine(lidx, addString(ch, chidx) _)
    Change(this, p, nav.moveHorizontally(p.>(ch)))
  }

  def revert(nav: Navigator) = {
    Change(this, nav.pointer, MolRemove(Option(String.valueOf(ch))).execute(nav).tp)
  }

  override def toString: String = s"CharInput(${String.valueOf(ch)})"
}

@key("ins")
case class Insert(t: String) extends Op {

  def execute(nav: Navigator) = {
    val Navigator(lines, meter, p@Indices(l, ch)) = nav
    import meter._
    lines.updateLine(l, addString(t, ch) _)
    Change(this, p, nav.moveHorizontally(p.>(t)))
  }

  def revert(nav: Navigator) = {
    Change(this, nav.pointer, MolRemove(Option(t)).execute(nav).tp)
  }
}

@key("pst")
case class Paste(lns: TreeMap[Int, (String, String)], @key("s") starts: Boolean) extends Op {

  def execute(nav: Navigator) = {
    val Navigator(lines, meter, fp) = nav
    import meter._
    val (start, (keepLeft, rmRight)) = lns.head
    val (end, (keepRight, rmLeft)) = lns.last

    lines.updateLine(start, str => keepLeft + rmRight)
    val toRecover = lns.dropRight(1).drop(1)
    toRecover.foreach { case (idx, (_, removed)) =>
      lines.insertLine(idx, removed)
    }
    lines.insertLine(end, rmLeft + keepRight)

    val tp =
      if (starts) {
        fp
      } else {
        nav.moveVertically(fp.<<.v(lns.size - 1).>(rmLeft))
      }
    Change(this, fp, tp)
  }

  def revert(nav: Navigator) = {
    Change(this, nav.pointer, Replace(lns, starts).execute(nav).tp)
  }
}

@key("rpl")
case class Replace(lns: TreeMap[Int, (String, String)], @key("s") starts: Boolean) extends Op {

  def execute(nav: Navigator) = {
    val Navigator(lines, meter, fp) = nav
    import meter._
    val (start, (keepLeft, _)) = lns.head
    val (_, (keepRight, _)) = lns.last
    lines.updateLine(start, str => keepLeft + keepRight)

    for (i <- 1 until lns.size) {
      lines.removeLine(start + 1)
    }
    val tp =
      if (starts) {
        fp
      } else {
        nav.moveVertically(fp.<<.^(lns.size - 1).>(keepLeft))
      }

    Change(this, fp, tp)
  }

  def revert(nav: Navigator) = {
    Change(this, nav.pointer, Paste(lns, starts).execute(nav).tp)
  }
}

object Change {
  implicit val treeMapR = IdbSupport.TreeMapR[Int, (String, String)]
  implicit val treeMapW = IdbSupport.TreeMapW[Int, (String, String)]

  implicit val ChangeW: Writer[Change] = Writer[Change](
    change => Js.Arr(
      writeJs[Op](change.op),
      Js.Num(change.fp.lidx), Js.Num(change.tp.lidx),
      Js.Num(change.fp.chidx), Js.Num(change.tp.chidx)
    )
  )

  implicit val ChangeR: Reader[Change] = Reader[Change] {
    case ch: Js.Arr =>
      val values = ch.value
      val op = readJs[Op](values(0))
      val fy = readJs[Int](values(1))
      val ty = readJs[Int](values(2))
      val fx = readJs[Int](values(3))
      val tx = readJs[Int](values(4))
      Change(op, Coord(fy, fx), Coord(ty, tx))
  }
}
