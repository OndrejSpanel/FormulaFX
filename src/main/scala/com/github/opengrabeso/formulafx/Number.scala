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

  def toMinutesPos(x: Double) = {
    assert(x >= 0)
    val degrees = x.toInt
    val minutes = (x - degrees) * 60
    val minutesWhole = minutes.toInt
    val minutesFrac = minutes - minutesWhole
    f"$degrees:$minutesWhole%02d${fractionString(minutesFrac, 5)}"
  }

  // TODO: DRY toMinutesPos / toSecondsPos
  def toSecondsPos(x: Double) = {
    val degrees = x.toInt
    val minutes = (x - degrees) * 60
    val minutesWhole = minutes.toInt
    val minutesFrac = minutes - minutesWhole
    val seconds = minutesFrac * 60
    val secondsWhole = seconds.toInt
    val secondsFrac = seconds - secondsWhole
    f"$degrees:$minutesWhole%02d:$secondsWhole%02d${fractionString(secondsFrac, 5)}"
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

  override def toString = f match {
    case Minutes => toMinutes(x)
    case Seconds => toSeconds(x)
    case _ => x.toString
  }
}
