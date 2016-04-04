package com.github.opengrabeso.formulafx


sealed trait AngleUnit {
  def toRadians(x: Double): Double
  def fromRadians(x: Double): Double
}
object AngleUnit {
  object Radian extends AngleUnit {
    override def toRadians(x: Double) = x
    override def fromRadians(x: Double) = x
    override def toString = "Radian"
  }
  object Degree extends AngleUnit{
    override def toRadians(x: Double) = x.toRadians
    override def fromRadians(x: Double) = x.toDegrees
    override def toString = "Degree"
  }
}

case class ExpressionSettings(angleUnit: AngleUnit)

trait Variables {
  def apply(name: String): Number
}

class Expression(variables: Variables, settings: ExpressionSettings) {

  sealed trait Item {
    def value: Number
  }

  class Variable(name: String) extends Item {
    def value = variables(name)
  }

  class Literal(number: Number) extends Item {
    def value = number
  }

  class Operator(op: (Double, Double) => Double, left: Item, right: Item) extends Item {
    def value = {
      val valueL = left.value
      val valueR = right.value
      val retValue = op(valueL.x, valueR.x)
      val retFormat = valueL combineFormat valueR
      Number(retValue, retFormat)
    }
  }

  class Function(f: Double => Number, x: Item) extends Item {
    def value = {
      val par = x.value
      f(par.x) // TODO: some functions should respect input format
    }
  }
}
