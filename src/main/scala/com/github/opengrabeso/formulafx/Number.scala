package com.github.opengrabeso.formulafx

sealed trait Format
object Format {
  object General extends Format

  object Minutes extends Format

  object Seconds extends Format
}

import Format._

object Number {
  def toMinutesPos(x: Double) = {
    assert(x >= 0)
    val mins = x.toInt
    val secs = (x - mins) * 60
    val secsWhole = secs.toInt
    // TODO: secs fracs
    //val secsFrac = secs - secsWhole
    //f"$mins:$secsWhole%02d$secsFrac%f"
    val secsFrac = secs - secsWhole
    f"$mins:$secsWhole%02d"
  }

  def toMinutes(x: Double): String = {
    if (x < 0) "-" + toMinutesPos(-x)
    else toMinutesPos(+x)
  }

  def toSeconds(x: Double) = {
    toMinutes(x)
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
