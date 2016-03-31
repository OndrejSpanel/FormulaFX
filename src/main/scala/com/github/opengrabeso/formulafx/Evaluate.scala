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

  private var variables = Map[String, Number]()
  sealed trait AngleUnit {
    def toRadians(x: Double): Double
    def fromRadians(x: Double): Double
  }
  object AngleUnit {
    object Radian extends AngleUnit {
      override def toRadians(x: Double) = x
      override def fromRadians(x: Double) = x
    }
    object Degree extends AngleUnit{
      override def toRadians(x: Double) = x.toRadians
      override def fromRadians(x: Double) = x.toDegrees
    }
  }

  private var angleUnit: AngleUnit = AngleUnit.Radian

  def angleUnitDegree(): Unit = angleUnit = AngleUnit.Degree

  def angleUnitRadian(): Unit = angleUnit = AngleUnit.Radian

  def angleToRadians(x: Double) = angleUnit.toRadians(x)
  def angleFromRadians(x: Double) = angleUnit.fromRadians(x)

  object ExprParser extends JavaTokenParsers {
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

    def function: Parser[Number] = parseFunctionName ~ ("(" ~> expr <~ ")") ^^ { case f ~ x => f(x.x) }

    def minutes: Parser[Number] = (wholeNumber <~ ":") ~ floatingPointNumber ^^ { case deg ~ min => deg.toInt + min.toDouble * (1.0 / 60) format Minutes }
    def minutesAndSeconds: Parser[Number] = (wholeNumber <~ ":") ~ (wholeNumber <~ ":") ~ floatingPointNumber ^^ {
      case (deg ~ min ~ sec) => deg.toInt + min.toInt * (1.0 / 60) + sec.toDouble * (1.0 / 3600) format Seconds
    }

    def hexNum: Parser[Number] = """0[xX][0-9a-fA-F]+""".r ^^ { s => Number(java.lang.Long.parseUnsignedLong(s.drop(2), 16), Hex) }

    def variable: Parser[Number] = ident ^^ { x => variables(x) }
    def fNumber: Parser[Number] = floatingPointNumber ^^ { x => x.toDouble }
    def number: Parser[Number] = minutesAndSeconds | minutes | hexNum | fNumber
    def factor: Parser[Number] = (number | function | variable) | "(" ~> expr <~ ")"


    def mulOperators: Parser[Operator] =
      operator("*" , _ * _) |
      operator("/" , _ / _)

    def addOperators: Parser[Operator] =
      operator("+", _ + _) |
      operator("-", _ - _)

    def callOperator(o: Operator, a: Number, b: Number): Number = Number(o(a.x, b.x), a combineFormat b)

    def processOperators: Number ~ List[Operator ~  Number] => Number = {
      case number ~ list => list.foldLeft(number) { case (x, op ~ y) => callOperator(op, x, y) }
    }

    def term: Parser[Number] = factor ~ rep(mulOperators ~ factor) ^^ processOperators

    def expr: Parser[Number] = term ~ rep(addOperators ~ term) ^^ processOperators

    def assign: Parser[Number] = (ident <~ "=") ~ expr ^^ {
      case i ~ x =>
        variables += (i -> x)
        x
    }

    def command = assign | expr

    def apply(input: String): Try[Number] = Try {parseAll(command, input)}.map {
      case Success(result, _) => result
      case failure: NoSuccess => throw new UnsupportedOperationException(failure.msg)
    }
  }

  def apply(input: String): Try[String] = {
    ExprParser(input).map(_.toString)
  }

}
