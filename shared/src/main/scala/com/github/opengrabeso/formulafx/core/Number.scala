package com.github.opengrabeso.formulafx.core

sealed trait Format {
  def score: Int

  def combine(that: Format) = if (that.score >= score) that else this
}

object Format {

  object General extends Format {
    def score = 10
  }

  object Hex extends Format {
    def score = 15
  }

  object Minutes extends Format {
    def score = 20
  }

  object Hours extends Format {
    def score = 30
  }

}

import Format._

object Number {
  def fractionString(x: Double, maxLen: Int): NumberPart = {

    def fractionFormatter(x: Double, digits: Int): String = {
      if (x == 0) ""
      else {
        assert(x.round == x)
        val xDigits = x.round.toInt.toString
        "." + "0" * (digits - xDigits.length) + xDigits
      }
    }

    def fractionPartParams(digits: Int) = NumberPartParams(1, Math.pow(10, digits), fractionFormatter(_, digits))

    val scale = Math.pow(10, maxLen)
    val raw = NumberPart((x * scale).round.toDouble, fractionPartParams(maxLen))

    def dropTailZeroes(p: NumberPart, maxLen: Int): NumberPart = {
      if (p.value % 10 == 0 && p.value > 0) {
        dropTailZeroes(p.copy(p.value / 10, fractionPartParams(maxLen-1)), maxLen-1)
      } else p
    }

    val ret = dropTailZeroes(raw, maxLen)
    ret
  }

  def extract60th(x: Double): (Int, Double) = {
    val minutes = x * 60
    val minutesWhole = minutes.toInt
    val minutesFrac = minutes - minutesWhole
    (minutesWhole, minutesFrac)
  }

  case class NumberPartParams(magnitude: Double, scale: Double, format: (Double) => String)

  case class NumberPart(value: Double, params: NumberPartParams) {
    override def toString = params.format(value)
  }

  case class NumberByParts(parts: List[NumberPart]) {
    override def toString = parts.mkString("")

    def doCarry: NumberByParts = {

      def carry(todo: List[NumberPart], isCarry: Boolean, done: List[NumberPart]): List[NumberPart] = {
        todo match {
          case Nil => done
          case head :: tail =>
            val withCarry = if (isCarry) head.copy(value = head.value + 1) else head
            val isCarryUp = withCarry.value >= withCarry.params.magnitude * withCarry.params.scale
            val overflown = if (isCarryUp) {
              withCarry.copy(value = withCarry.value - withCarry.params.magnitude)
            } else withCarry

            carry(tail, isCarryUp, overflown +: done)
        }
      }

      NumberByParts(carry(parts.reverse, false, Nil))
    }

  }

  val intPart = NumberPartParams(Int.MaxValue, 1, x => f"${x.toInt}")
  val minSecPart = NumberPartParams(60, 1, x => f":${x.toInt}%02d")

  def toMinutesPos(xSec: Double) = {
    assert(xSec >= 0)
    if (xSec>Int.MaxValue) xSec.toString
    else {
      val x = xSec / 60
      val degrees = x.toInt
      val (minutesWhole, minutesFrac) = extract60th(x - degrees)

      val formatted = NumberByParts(List(
        NumberPart(degrees, intPart),
        NumberPart(minutesWhole, minSecPart),
        fractionString(minutesFrac, 5)
      ))

      formatted.doCarry.toString
    }
  }

  def toSecondsPos(xSec: Double) = {
    if (xSec>Int.MaxValue) xSec.toString
    else {
      val x = xSec / 3600
      val degrees = x.toInt
      val (minutesWhole, minutesFrac) = extract60th(x - degrees)
      val (secondsWhole, secondsFrac) = extract60th(minutesFrac)

      val formatted = NumberByParts(List(
        NumberPart(degrees, intPart),
        NumberPart(minutesWhole, minSecPart),
        NumberPart(secondsWhole, minSecPart),
        fractionString(secondsFrac, 5)
      ))

      formatted.doCarry.toString
    }
  }

  def toMinutes(x: Double): String = {
    if (x < 0) "-" + toMinutesPos(-x)
    else toMinutesPos(+x)
  }

  def toHours(x: Double) = {
    if (x < 0) "-" + toSecondsPos(-x)
    else toSecondsPos(+x)
  }

  def toHex(x: Double) = {
    // TODO: scientific and fractional hex formats
    if (x.round != x) {
      x.toString
    } else {
      f"0x${x.round}%x"
    }
  }

}

case class Number(x: Double, f: Format) {
  import Number._

  def combineFormat(b: Number): Format = f combine b.f

  override def toString = f match {
    case Minutes => toMinutes(x)
    case Hours => toHours(x)
    case Hex => toHex(x)
    case _ => x.toString
  }
}
