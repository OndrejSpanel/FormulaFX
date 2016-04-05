package com.github.opengrabeso.formulafx

import scala.language.implicitConversions
import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

object Evaluate {
  import Format._

  implicit def doubleToNumber(x: Double): Number = Number(x, General)
  implicit class FormattedNumber(val x: Double) extends AnyVal {
    def format(f: Format) = Number(x, f)
  }

  private var variableStore = collection.mutable.Map[String, Number]()

  private var angleUnit: AngleUnit = AngleUnit.Radian

  def clear(): Unit = {variableStore = collection.mutable.Map()}

  def angleUnitDegree(): Unit = angleUnit = AngleUnit.Degree
  def angleUnitRadian(): Unit = angleUnit = AngleUnit.Radian
  def angleUnitName: String = angleUnit.toString

  def angleUnitIsDegree: Boolean = angleUnit == AngleUnit.Degree

  def angleUnitIsRadian: Boolean = angleUnit == AngleUnit.Radian

  def angleToRadians(x: Double) = angleUnit.toRadians(x)
  def angleFromRadians(x: Double) = angleUnit.fromRadians(x)

  class ExprParser()(implicit val settings: ExpressionSettings) extends JavaTokenParsers with Expression {

    type Operator = Expression.Operator
    type Function = Expression.Function

    def operator[T](p: Parser[T], v: => Operator): Parser[Operator] = p ^^^ v
    def function[T](p: Parser[T], v: => Function): Parser[Function] = p ^^^ v

    def funcParAngle(v: Function): Function = new Function {
      override def apply(v1: Double): Number = v.apply(angleToRadians(v1))
      override def inverse(ret: Double): Double = angleFromRadians(v.inverse(ret))
    }
    def funcRetAngle(v: Function): Function = new Function {
      override def apply(v1: Double): Number = angleFromRadians(v.apply(v1).x)
      override def inverse(ret: Double): Double = v.inverse(angleToRadians(ret))
    }

    def functionParAngle[T](p: Parser[T], v: Function): Parser[Function] = p ^^^ funcParAngle(v)
    def functionRetAngle[T](p: Parser[T], v: Function): Parser[Function] = p ^^^ funcRetAngle(v)

    def parseFunctionName: Parser[Function] =
      function("exp", Expression.function_exp) |
        function("ln", Expression.function_ln) |
        functionParAngle("sin", Expression.function_sin) |
        functionParAngle("cos", Expression.function_cos) |
        functionParAngle("tan", Expression.function_tan) |
        functionRetAngle("asin", Expression.function_asin) |
        functionRetAngle("acos", Expression.function_acos) |
        functionRetAngle("atan", Expression.function_atan) |
        function("sqrt", Expression.function_sqrt) |
        function("log", Expression.function_log) |
        function("floor", Expression.function_floor) |
        function("ceil", Expression.function_ceil) |
        function("round", Expression.function_round) |
        function("signum", Expression.function_signum) |
        function("abs", Expression.function_abs) |
        functionParAngle("sinh", Expression.function_sinh) |
        functionParAngle("cosh", Expression.function_cosh) |
        functionParAngle("tanh", Expression.function_tanh) |
        function("hex", Expression.function_hex)

    def function: Parser[FunctionItem] = parseFunctionName ~ ("(" ~> expr <~ ")") ^^ { case f ~ x => new FunctionItem(f, x) }

    implicit def numberToItem(x: Number): LiteralItem = new LiteralItem(x)
    implicit def doubleToItem(x: Double): LiteralItem = new LiteralItem(Number(x, General))

    def minutes: Parser[LiteralItem] = (wholeNumber <~ ":") ~ floatingPointNumber ^^ { case deg ~ min => deg.toInt + min.toDouble * (1.0 / 60) format Minutes }
    def minutesAndSeconds: Parser[LiteralItem] = (wholeNumber <~ ":") ~ (wholeNumber <~ ":") ~ floatingPointNumber ^^ {
      case (deg ~ min ~ sec) => deg.toInt + min.toInt * (1.0 / 60) + sec.toDouble * (1.0 / 3600) format Seconds
    }

    def hexNum: Parser[LiteralItem] = """0[xX][0-9a-fA-F]+""".r ^^ { s => Number(java.lang.Long.parseUnsignedLong(s.drop(2), 16), Hex) }

    def variable: Parser[VariableItem] = ident ^^ { x => new VariableItem(x) }
    def fNumber: Parser[LiteralItem] = floatingPointNumber ^^ { x => x.toDouble }
    def number: Parser[LiteralItem] = minutesAndSeconds | minutes | hexNum | fNumber
    def factor: Parser[Item] = (number | function | variable) | "(" ~> expr <~ ")"

    def powOperators =
      operator("^" , Expression.operator_^)

    def mulOperators =
      operator("*" , Expression.operator_*) |
      operator("/" , Expression.operator_/)

    def addOperators =
      operator("+", Expression.operator_+) |
      operator("-", Expression.operator_-)

    def processOperators: Item ~ List[Operator ~  Item] => Item = {
      case number ~ list => list.foldLeft(number) { case (x, op ~ y) => new OperatorItem(op, x, y) }
    }

    def powTerm: Parser[Item] = factor ~ rep(powOperators ~ factor) ^^ processOperators

    def term: Parser[Item] = powTerm ~ rep(mulOperators ~ powTerm) ^^ processOperators

    def expr: Parser[Item] = term ~ rep(addOperators ~ term) ^^ processOperators

    def assign: Parser[Number] = (expr <~ "=") ~ expr ^^ {
      case i ~ x =>
        val (iSolved, res) = solve(i, x)
        iSolved match {
          case VariableItem(varName) =>
            if (!settings.preview) {
              settings.variables += (varName -> res)
            }
            res
          case _ =>
            throw new UnsupportedOperationException("Unable to solve equation")
        }
    }

    def evaluateExpr: Parser[Number] = expr ^^ {_.value}

    def command = assign | evaluateExpr

    def apply(input: String): Try[Number] = Try {parseAll(command, input)}.map {
      case Success(result, _) => result
      case failure: NoSuccess => throw new UnsupportedOperationException(failure.msg)
    }
  }

  object ExprParser extends ExprParser()(
    new ExpressionSettings(AngleUnit.Radian, false, collection.mutable.Map.empty)
  )

  def compute(input: String, preview: Boolean): Try[String] = {
    val exprParser = new ExprParser()(
      new ExpressionSettings(angleUnit, preview, variableStore)
    )

    exprParser(input).map(_.toString)
  }
}
