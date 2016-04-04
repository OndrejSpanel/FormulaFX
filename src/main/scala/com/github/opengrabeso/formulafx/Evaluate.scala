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

  private var variableStore = Map[String, Number]()

  private var angleUnit: AngleUnit = AngleUnit.Radian

  def clear(): Unit = {variableStore = Map()}

  def angleUnitDegree(): Unit = angleUnit = AngleUnit.Degree
  def angleUnitRadian(): Unit = angleUnit = AngleUnit.Radian
  def angleUnitName: String = angleUnit.toString

  def angleUnitIsDegree: Boolean = angleUnit == AngleUnit.Degree

  def angleUnitIsRadian: Boolean = angleUnit == AngleUnit.Radian

  def angleToRadians(x: Double) = angleUnit.toRadians(x)
  def angleFromRadians(x: Double) = angleUnit.fromRadians(x)

  class ExprParser()(implicit val settings: ExpressionSettings) extends JavaTokenParsers with Expression {

    type Operator = (Double, Double) => Double
    type Function = Double => Number

    def operator[T](p: Parser[T], v: => Operator): Parser[Operator] = p ^^^ v
    def function[T](p: Parser[T], v: => Function): Parser[Function] = p ^^^ v

    def functionDouble[T](p: Parser[T], v: => Double => Double): Parser[Function] = p ^^^ {x => Number(v(x), General)}
    def functionParAngle[T](p: Parser[T], v: => Double => Double): Parser[Function] = p ^^^ {x => Number(v(angleToRadians(x)), General)}
    def functionRetAngle[T](p: Parser[T], v: => Double => Double): Parser[Function] = p ^^^ {x => Number(angleFromRadians(v(x)), General)}

    def parseFunctionName: Parser[Function] =
      functionParAngle("sin", Math.sin) |
        functionParAngle("cos", Math.cos) |
        functionParAngle("tan", Math.tan) |
        functionRetAngle("asin", Math.asin) |
        functionRetAngle("acos", Math.acos) |
        functionRetAngle("atan", Math.atan) |
        functionParAngle("sinh", Math.sinh) |
        functionParAngle("cosh", Math.cosh) |
        functionParAngle("tanh", Math.tanh) |
        function("exp", Math.exp) |
        function("ln", Math.log) |
        function("log", Math.log10) |
        function("sqrt", Math.sqrt) |
        function("floor", Math.floor) |
        function("ceil", Math.ceil) |
        functionDouble("round", x => Math.round(x)) |
        functionDouble("abs", Math.abs) |
        functionDouble("signum", Math.signum) |
        function("hex", x => Number(x, Hex))

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


    def mulOperators =
      operator("*" , _ * _) |
      operator("/" , _ / _)

    def addOperators =
      operator("+", _ + _) |
      operator("-", _ - _)

    def processOperators: Item ~ List[Operator ~  Item] => Item = {
      case number ~ list => list.foldLeft(number) { case (x, op ~ y) => new OperatorItem(op, x, y) }
    }

    def term: Parser[Item] = factor ~ rep(mulOperators ~ factor) ^^ processOperators

    def expr: Parser[Item] = term ~ rep(addOperators ~ term) ^^ processOperators

    def assign: Parser[Number] = (expr <~ "=") ~ expr ^^ {
      case i ~ x =>
        val (iSolved, xSolved) = solve(i, x)
        iSolved match {
          case VariableItem(varName) =>
            val res = xSolved.value
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
    new ExpressionSettings(AngleUnit.Radian, false, Map.empty)
  )

  def compute(input: String, preview: Boolean): Try[String] = {
    val exprParser = new ExprParser()(
      new ExpressionSettings(angleUnit, preview, variableStore)
    )

    exprParser(input).map(_.toString)
  }
}
