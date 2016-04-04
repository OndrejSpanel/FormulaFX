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

trait Variables extends PartialFunction[String, Number] {
  def apply(name: String): Number
  def isDefinedAt(name: String): Boolean
}

trait Expression {
  def variables: Variables
  def settings: ExpressionSettings

  sealed trait Item {
    def isConstant: Boolean
    def value: Number
  }

  case class VariableItem(name: String) extends Item {
    def isConstant = variables.isDefinedAt(name)
    def value = variables(name)
  }

  case class LiteralItem(number: Number) extends Item {
    def value = number
    def isConstant = true
  }

  case class OperatorItem(op: (Double, Double) => Double, left: Item, right: Item) extends Item {
    def value = {
      val valueL = left.value
      val valueR = right.value
      val retValue = op(valueL.x, valueR.x)
      val retFormat = valueL combineFormat valueR
      Number(retValue, retFormat)
    }
    def isConstant = left.isConstant && right.isConstant
  }

  case class FunctionItem(f: Double => Number, x: Item) extends Item {
    def value = {
      val par = x.value
      f(par.x) // TODO: some functions should respect input format
    }
    def isConstant = x.isConstant
  }

  def solve(left: Item, right: Item): (Item, Item) = {
    left -> right
  }

}
