package com.github.opengrabeso.formulafx

sealed trait Format {
  def score: Int
}

object Format {

  object General extends Format {
    def score = 10
  }

  object Minutes extends Format {
    def score = 20
  }

  object Seconds extends Format {
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

    def fractionPartParams(digits: Int) = NumberPartParams(1, 1 / Math.pow(10, digits), fractionFormatter(_, digits))

    val scale = Math.pow(10, maxLen)
    val raw = NumberPart((x * scale).round, fractionPartParams(maxLen))

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

  case class NumberByParts(parts: Seq[NumberPart]) {
    override def toString = parts.mkString("")

    def roundToFixed(fracDigits: Int) = {
      val smallFirst = parts.reverse
      val rounding = smallFirst.head

      val precision = rounding.params.magnitude * Math.pow(0.1, fracDigits)

      rounding.value
    }

    def roundToSignificant(significantDigits: Int) = this
  }

  val intPart = NumberPartParams(Int.MaxValue, 1, x => f"${x.toInt}")
  val minSecPart = NumberPartParams(60, 1, x => f":${x.toInt}%02d")

  def toMinutesPos(x: Double) = {
    assert(x >= 0)
    if (x>Int.MaxValue) x.toString
    else {
      val degrees = x.toInt
      val (minutesWhole, minutesFrac) = extract60th(x - degrees)

      val formatted = NumberByParts(Seq(
        NumberPart(degrees, intPart),
        NumberPart(minutesWhole, minSecPart),
        fractionString(minutesFrac, 5)
      ))

      formatted.toString
    }
  }

  def toSecondsPos(x: Double) = {
    if (x>Int.MaxValue) x.toString
    else {
      val degrees = x.toInt
      val (minutesWhole, minutesFrac) = extract60th(x - degrees)
      val (secondsWhole, secondsFrac) = extract60th(minutesFrac)

      val formatted = NumberByParts(Seq(
        NumberPart(degrees, intPart),
        NumberPart(minutesWhole, minSecPart),
        NumberPart(secondsWhole, minSecPart),
        fractionString(secondsFrac, 5)
      ))

      formatted.toString
    }
  }

  def toMinutes(x: Double): String = {
    if (x < 0) "-" + toMinutesPos(-x)
    else toMinutesPos(+x)
  }

  def toSeconds(x: Double) = {
    if (x < 0) "-" + toSecondsPos(-x)
    else toSecondsPos(+x)
  }

}

case class Number(x: Double, f: Format) {
  import Number._

  def combineFormat(b: Number): Format = if (f.score >= b.f.score) f else b.f

  override def toString = f match {
    case Minutes => toMinutes(x)
    case Seconds => toSeconds(x)
    case _ => x.toString
  }
}
