package com.viagraphs.replayed

import derive.key

import upickle.legacy._
import upickle.Js

import scala.collection.immutable.TreeMap

sealed trait Foo {
  def execute(nav: Navigator): Change
}

@key("eolDel")
case object FooImpl extends Foo {
  override def execute(nav: Navigator): Change = ???
}

@key("rpl")
case class Replace2(lns: TreeMap[Int, (String, String)], @key("s") starts: Boolean) extends Foo {

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

    Change(null, fp, tp)
  }

  def revert(nav: Navigator) = {
    Change(null, nav.pointer, Paste(lns, starts).execute(nav).tp)
  }
}

@key("molDel")
case class BarImpl(t: Option[String] = None) extends Foo {
  override def execute(nav: Navigator): Change = ???
}

object App {

  def write(foo: Foo): Js.Value = {
    writeJs[Foo](foo)
  }

  implicit val ChangeW: Writer[Foo] = Writer[Foo](
    change => Js.Arr(
      writeJs[Foo](change)
    )
  )

  write(FooImpl)
}

