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

object Expression {
  type Variables = collection.mutable.Map[String, Number]


  trait Operator extends ((Double, Double) => Double) {
    def inverseLeft(ret: Double, v1: Double): Double
    def inverseRight(ret: Double, v2: Double): Double
  }

  object operator_+ extends Operator {
    override def apply(v1: Double, v2: Double) = v1 + v2
    override def inverseLeft(ret: Double, v1: Double) = ret - v1
    override def inverseRight(ret: Double, v2: Double) = ret - v2
  }
  object operator_- extends Operator {
    override def apply(v1: Double, v2: Double) = v1 - v2
    override def inverseLeft(ret: Double, v1: Double) = v1 - ret
    override def inverseRight(ret: Double, v2: Double) = ret + v2
  }
  object operator_* extends Operator {
    override def apply(v1: Double, v2: Double) = v1 * v2
    override def inverseLeft(ret: Double, v1: Double) = ret / v1
    override def inverseRight(ret: Double, v2: Double) = ret / v2
  }
  object operator_/ extends Operator {
    override def apply(v1: Double, v2: Double) = v1 / v2
    override def inverseLeft(ret: Double, v1: Double) = v1 / ret
    override def inverseRight(ret: Double, v1: Double) = ret * v1
  }
}

import Expression._

case class ExpressionSettings(angleUnit: AngleUnit, preview: Boolean, variables: Variables)

trait Expression {
  expr =>

  sealed trait Item {
    def isConstant(implicit settings: ExpressionSettings): Boolean
    def value(implicit settings: ExpressionSettings): Number
    def leftmostVariable: Option[String]
  }

  case class VariableItem(name: String) extends Item {
    def isConstant(implicit settings: ExpressionSettings) = settings.variables.isDefinedAt(name)
    def value(implicit settings: ExpressionSettings) = settings.variables(name)
    def leftmostVariable = Some(name)
  }

  case class LiteralItem(number: Number) extends Item {
    def value(implicit settings: ExpressionSettings) = number
    def isConstant(implicit settings: ExpressionSettings) = true
    def leftmostVariable = None
  }

  case class OperatorItem(op: Expression.Operator, left: Item, right: Item) extends Item {
    def value(implicit settings: ExpressionSettings) = {
      val valueL = left.value
      val valueR = right.value
      val retValue = op(valueL.x, valueR.x)
      val retFormat = valueL combineFormat valueR
      Number(retValue, retFormat)
    }
    def isConstant(implicit settings: ExpressionSettings) = left.isConstant && right.isConstant
    def leftmostVariable = left.leftmostVariable orElse right.leftmostVariable
  }

  case class FunctionItem(f: Double => Number, x: Item) extends Item {
    def value(implicit settings: ExpressionSettings) = {
      val par = x.value
      f(par.x) // TODO: some functions should respect input format
    }
    def isConstant(implicit settings: ExpressionSettings) = x.isConstant
    def leftmostVariable = x.leftmostVariable
  }

  private def solveLeftUnknown(left: Item, right: Number)(implicit settings: ExpressionSettings): (Item, Number) = {
    def computeOp(op: (Double, Double) => Double, a: Number, b: Number): Number = {
      Number(op(a.x, b.x), a combineFormat b)
    }
    left match {
      case OperatorItem(op, a, b) =>
        // a op b = right
        val (newLeft, newRight) = (a.isConstant, b.isConstant) match {
          case (true, true) => throw new UnsupportedOperationException("No unknown detected")
          case (false, true) => a -> computeOp(op.inverseRight, right, b.value)
          case (true, false) => b -> computeOp(op.inverseLeft, right, a.value)
          case (false, false) => throw new UnsupportedOperationException("Two unknowns encountered")
        }
        solveLeftUnknown(newLeft, newRight)
      case _ =>
        (left, right)
    }
  }

  def solveWithUnknown(left: Item, right: Item, unknownName: String)(implicit settings: ExpressionSettings): (Item, Number) = {
    val settingsWithUnknown = settings.copy(variables = settings.variables - unknownName)
    solve(left, right)(settingsWithUnknown)
  }

  def solve(left: Item, right: Item)(implicit settings: ExpressionSettings): (Item, Number) = {
    (left.isConstant, right.isConstant) match {
      case (false, true) =>
        solveLeftUnknown(left, right.value)
      case (true, false) =>
        solveLeftUnknown(right, left.value)
      case (false, false) =>
        throw new UnsupportedOperationException("Equation with mutliple unknowns") // TODO: allow multiple occurences of one unknown
      case (true, true) =>
        // determine "most unknown" automatically
        val leftUnknown = left.leftmostVariable
        val rightUnknown = right.leftmostVariable
        val unknown = leftUnknown orElse rightUnknown
        unknown.map { unknownName =>
          // make the variable unknown, try again
          solveWithUnknown(left, right, unknownName)
        }.getOrElse {
          throw new UnsupportedOperationException("Equation with no unknown")
        }
    }
  }

}
