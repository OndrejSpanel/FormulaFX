package com.github.opengrabeso.formulafx.core

import Format._

import scala.language.implicitConversions
import scala.util.chaining._

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
    infix def combineFormat(l: Format, r: Format) = l combine r
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
    override def apply(v1: Double, v2: Double) = v1 / v2    // ret = v1 / v2
    override def inverseLeft(ret: Double, v1: Double) = v1 / ret
    override def inverseRight(ret: Double, v2: Double) = ret * v2
    infix override def combineFormat(l: Format, r: Format) = {
      if (l == r) Format.General
      else super.combineFormat(l, r)
    }
  }
  object operator_^ extends Operator {
    override def apply(v1: Double, v2: Double) = Math.pow(v1, v2) // ret = exp(log(v1) * v2), log(ret) = log(v1) * v2
    override def inverseLeft(ret: Double, v1: Double) = Math.log(ret) / Math.log(v1)
    override def inverseRight(ret: Double, v2: Double) = Math.pow(ret, 1/v2) // log(v1) = log(ret)/v2
  }

  trait Function extends (Double => Number) {
    def inverse(ret: Double): Double
  }

  class FunctionNoInverse(f: Double => Number) extends Function {
    def apply(v1: Double) = f(v1)
    def inverse(ret: Double): Double = throw new UnsupportedOperationException("Unable to invert function")
  }

  implicit def doubleToNumber(x: Double): Number = Number(x, General)

  object function_sin extends Function {
    override def apply(v1: Double) = Math.sin(v1)
    override def inverse(ret: Double) = Math.asin(ret)
  }
  object function_cos extends Function {
    override def apply(v1: Double) = Math.cos(v1)
    override def inverse(ret: Double) = Math.acos(ret)
  }
  object function_tan extends Function {
    override def apply(v1: Double) = Math.tan(v1)
    override def inverse(ret: Double) = Math.atan(ret)
  }
  object function_asin extends Function {
    override def apply(v1: Double) = Math.asin(v1)
    override def inverse(ret: Double) = Math.sin(ret)
  }
  object function_acos extends Function {
    override def apply(v1: Double) = Math.acos(v1)
    override def inverse(ret: Double) = Math.cos(ret)
  }
  object function_atan extends Function {
    override def apply(v1: Double) = Math.atan(v1)
    override def inverse(ret: Double) = Math.tan(ret)
  }

  object function_exp extends Function {
    override def apply(v1: Double) = Math.exp(v1)
    override def inverse(ret: Double) = Math.log(ret)
  }
  object function_ln extends Function {
    override def apply(v1: Double) = Math.log(v1)
    override def inverse(ret: Double) = Math.exp(ret)
  }

  object function_log extends Function {
    override def apply(v1: Double) = Math.log10(v1)
    override def inverse(ret: Double) = Math.pow(10, ret)
  }

  object function_sqrt extends Function {
    override def apply(v1: Double) = Math.sqrt(v1)
    override def inverse(ret: Double) = ret * ret
  }

  object function_abs extends Function {
    override def apply(v1: Double) = Math.abs(v1)
    override def inverse(ret: Double) = if (ret<0) throw new UnsupportedOperationException("Abs cannot be negative") else ret
  }

  object function_floor extends FunctionNoInverse(Math.floor)
  object function_ceil extends FunctionNoInverse(Math.ceil)
  object function_round extends FunctionNoInverse(Math.round(_).toDouble)
  object function_signum extends FunctionNoInverse(x => Math.signum(x))

  object function_sinh extends FunctionNoInverse(Math.sinh)
  object function_cosh extends FunctionNoInverse(Math.cosh)
  object function_tanh extends FunctionNoInverse(Math.tanh)

  object function_hex extends Function {
    override def apply(v1: Double) = Number(v1, Hex)
    override def inverse(ret: Double) = ret
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
      val retFormat = op.combineFormat(valueL.f, valueR.f)
      Number(retValue, retFormat)
    }
    def isConstant(implicit settings: ExpressionSettings) = left.isConstant && right.isConstant
    def leftmostVariable = left.leftmostVariable orElse right.leftmostVariable
  }

  case class FunctionItem(f: Function, x: Item) extends Item {
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
      case FunctionItem(f, x) =>
        assert(!x.isConstant)
        val newRight = f.inverse(right.x)
        solveLeftUnknown(x, newRight)
      case _ =>
        (left, right)
    }
  }

  def solveWithUnknown(left: Item, right: Item, unknownName: String)(implicit settings: ExpressionSettings): (Item, Number) = {
    val settingsWithUnknown = settings.copy(variables = settings.variables.clone().tap(_ -= unknownName))
    solve(left, right)(settingsWithUnknown)
  }

  def solve(left: Item, right: Item)(implicit settings: ExpressionSettings): (Item, Number) = {
    (left.isConstant, right.isConstant) match {
      case (false, true) =>
        solveLeftUnknown(left, right.value)
      case (true, false) =>
        solveLeftUnknown(right, left.value)
      case (false, false) =>
        throw new UnsupportedOperationException("Equation with multiple unknowns") // TODO: allow multiple occurences of one unknown
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
