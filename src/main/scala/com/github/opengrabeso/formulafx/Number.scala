package com.github.opengrabeso.formulafx

sealed trait Format
object Format {
  object General extends Format

  object Minutes extends Format

  object Seconds extends Format
}

import Format._

case class Number(x: Double, f: Format) {
  override def toString = f match {
    case Minutes => "M:" + x.toString
    case Seconds => "S:" + x.toString
    case _ => x.toString
  }
}
