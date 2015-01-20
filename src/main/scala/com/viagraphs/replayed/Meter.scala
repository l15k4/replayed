package com.viagraphs.replayed

import com.viagraphs.scalajs.dom.ChCode

import scala.collection.mutable


class Meter private(lines: Lines) {
  import scala.language.implicitConversions

  implicit def Ch2ChArr(ch: Char): Array[Char] = Array(ch)
  implicit def S2ChArr(string: String): Array[Char] = string.toCharArray
  implicit def Ch2S(ch: Char): String = Character.toString(ch)

  implicit def Ch2Width(ch: Char): Array[Double] = memoizedMeasure(Array(ch))
  implicit def ChArr2Width(chars: Array[Char]): Array[Double] = memoizedMeasure(chars)
  implicit def S2Width(string: String): Array[Double] = memoizedMeasure(string.toCharArray)

  private def measureChars(chars: Traversable[Int]): mutable.Map[Int, Double] = {
    val in = lines.le
    val under = lines.createLineDiv
    require(chars.nonEmpty, "Do not call me to measure nothing !")
    val spansWithChar = chars.map { char =>
      val spanEl = lines.createLineSpan
      spanEl.textContent = String.valueOf(char.toChar)
      (char, spanEl)
    }

    spansWithChar.foreach { case (ch, s) =>
      under.appendChild(s)
    }

    in.appendChild(under)

    val result = spansWithChar.foldLeft(mutable.Map[Int, Double]()) { case (acc, (ch, s)) =>
      acc.put(ch, s.getBoundingClientRect().width)
      acc
    }

    in.removeChild(under)
    result
  }

  def memoizedMeasure(chars: Array[Char]): Array[Double] = {
    def measure(ch: Char): Double = Meter.charWidths.lift(ch).getOrElse {
      def measureChar(ch: Int) = {
        val in = lines.le
        val under = lines.createLineDiv
        val span = lines.createLineSpan
        span.textContent = String.valueOf(ch.toChar)
        under.appendChild(span)
        in.appendChild(under)
        val result = span.getBoundingClientRect().width
        in.removeChild(under)
        result
      }
      val newCharSize = measureChar(ch)
      Meter.charWidths.put(ch, newCharSize)
      newCharSize
    }
    if (chars.isEmpty)
      return Array()
    else if (chars.size == 1)
      return Array(measure(chars(0)))

    val known = mutable.Map[Int, Double]()
    val unknown = chars.foldLeft(List[Int]()) { (acc, char) =>
      Meter.charWidths.lift(char) match {
        case Some(size) =>
          known.put(char, size)
          acc
        case None =>
          char :: acc
      }
    }

    if (unknown.nonEmpty) {
      val newSizes = measureChars(unknown)
      Meter.charWidths ++= newSizes
      known ++= newSizes
    }

    chars.map(known(_))
  }

}

object Meter {
  val charWidths: mutable.Map[Int, Double] = mutable.Map()

  def apply(lines: Lines): Meter = {
    val meter = new Meter(lines)
    if (Meter.charWidths.isEmpty) {
      Meter.charWidths ++= {
        val defaults = ChCode.shiftableKey2Char.values.map ( shift =>
          List(shift(false), shift(true))
        ).flatten.++(ChCode.key2char.values)
        meter.measureChars(defaults)
      }
    }
    meter
  }
}