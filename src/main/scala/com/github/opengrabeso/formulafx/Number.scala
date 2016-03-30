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

import scala.annotation.tailrec

object Number {
  def fractionString(x: Double, maxLen: Int): String = {

    @tailrec
    def fractionStringRecursive(x: Double, maxLen: Int, res: StringBuilder): StringBuilder = {
      val threshold = Math.pow(0.1, maxLen)
      if (x<threshold || maxLen <=0) {
        assert(x<threshold) // when maxLen is 0, rest should be 10 and this should always pass
        res
      } else {
        assert(x >= 0 && x < 1)
        val digit = (x * 10).toInt
        val rest = x * 10 - digit
        val digitChar = ('0' + digit).toChar
        fractionStringRecursive(rest, maxLen - 1, res + digitChar)
      }
    }

    // TODO: round to max digits
    val fractionalDigits = fractionStringRecursive(x, maxLen, StringBuilder.newBuilder).toString
    if (fractionalDigits.nonEmpty) "." + fractionalDigits
    else ""
  }

  def extract60th(x: Double): (Int, Double) = {
    val minutes = x * 60
    val minutesWhole = minutes.toInt
    val minutesFrac = minutes - minutesWhole
    (minutesWhole, minutesFrac)
  }

  case class NumberPartParams(magnitude: Double, format: (Double) => String)

  case class NumberPart(value: Double, params: NumberPartParams) {
    override def toString = params.format(value)
  }

  val intPart = NumberPartParams(Int.MaxValue, x => f"${x.toInt}")
  val minSecPart = NumberPartParams(60, x => f":${x.toInt}%02d")
  val fracPart = NumberPartParams(1, fractionString(_, 5))

  def toMinutesPos(x: Double) = {
    assert(x >= 0)
    if (x>Int.MaxValue) x.toString
    else {
      val degrees = x.toInt
      val (minutesWhole, minutesFrac) = extract60th(x - degrees)

      val formatted = Seq(
        NumberPart(degrees, intPart),
        NumberPart(minutesWhole, minSecPart),
        NumberPart(minutesFrac, fracPart)
      )

      formatted.mkString("")
    }
  }

  def toSecondsPos(x: Double) = {
    if (x>Int.MaxValue) x.toString
    else {
      val degrees = x.toInt
      val (minutesWhole, minutesFrac) = extract60th(x - degrees)
      val (secondsWhole, secondsFrac) = extract60th(minutesFrac)

      val formatted = Seq(
        NumberPart(degrees, intPart),
        NumberPart(minutesWhole, minSecPart),
        NumberPart(secondsWhole, minSecPart),
        NumberPart(secondsFrac, fracPart)
      )

      formatted.mkString("")
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
