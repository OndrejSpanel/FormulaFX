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

  var variables = Map[String, Number]()

  object ExprParser extends JavaTokenParsers {
    type Operator = (Double, Double) => Double
    type Function = Double => Double

    def operator[T](p: Parser[T], v: => Operator): Parser[Operator] = p ^^^ v
    def function[T](p: Parser[T], v: => Function): Parser[Function] = p ^^^ v

    def parseFunctionName: Parser[Function] =
        function("sin", Math.sin) |
        function("cos", Math.cos) |
        function("tan", Math.tan) |
        function("asin", Math.asin) |
        function("acos", Math.acos) |
        function("atan", Math.atan) |
        function("exp", Math.exp) |
        function("ln", Math.log) |
        function("log", Math.log10) |
        function("sqrt", Math.sqrt) |
        function("floor", Math.floor) |
        function("ceil", Math.ceil) |
        function("round", x => Math.round(x)) |
        function("abs", Math.abs) |
        function("signum", Math.signum) |
        function("sinh", Math.sinh) |
        function("cosh", Math.cosh) |
        function("tanh", Math.tanh)

    def function: Parser[Number] = parseFunctionName ~ ("(" ~> expr <~ ")") ^^ { case f ~ x => f(x.x) }

    def minutes: Parser[Number] = (wholeNumber <~ ":") ~ floatingPointNumber ^^ { case deg ~ min => deg.toInt + min.toDouble * (1.0 / 60) format Minutes }
    def minutesAndSeconds: Parser[Number] = (wholeNumber <~ ":") ~ (wholeNumber <~ ":") ~ floatingPointNumber ^^ {
      case (deg ~ min ~ sec) => deg.toInt + min.toInt * (1.0 / 60) + sec.toDouble * (1.0 / 3600) format Seconds
    }

    def hexNum: Parser[Number] = """0[xX][0-9a-fA-F]+""".r ^^ { s => Number(java.lang.Long.parseUnsignedLong(s.drop(2), 16), General) }

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
