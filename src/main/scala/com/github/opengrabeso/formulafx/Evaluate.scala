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

    def parseFunctionName: Parser[Function] =
      "sin" ^^^ Math.sin _ |
        "cos" ^^^ Math.cos _ |
        "tan" ^^^ Math.tan _ |
        "asin" ^^^ Math.asin _ |
        "acos" ^^^ Math.acos _ |
        "atan" ^^^ Math.atan _ |
        "exp" ^^^ Math.exp _ |
        "ln" ^^^ Math.log _ |
        "log" ^^^ Math.log10 _ |
        "sqrt" ^^^ Math.sqrt _ |
        "floor" ^^^ Math.floor _ |
        "ceil" ^^^ Math.ceil _ |
        "round" ^^^ {(x : Double) => Math.round(x).toDouble} |
        "abs" ^^^ {(x : Double) => Math.abs(x) } |
        "signum" ^^^ {(x : Double) => Math.signum(x) } |
        "sinh" ^^^ Math.sinh _ |
        "cosh" ^^^ Math.cosh _ |
        "tanh" ^^^ Math.tanh _

    def function: Parser[Number] = parseFunctionName ~ ("(" ~> expr <~ ")") ^^ { case f ~ x => f(x.x) }

    def minutes: Parser[Number] = (wholeNumber <~ ":") ~ floatingPointNumber ^^ { case deg ~ min => deg.toInt + min.toDouble * (1.0 / 60) format Minutes }
    def minutesAndSeconds: Parser[Number] = (wholeNumber <~ ":") ~ (wholeNumber <~ ":") ~ floatingPointNumber ^^ {
      case (deg ~ min ~ sec) => deg.toInt + min.toInt * (1.0 / 60) + sec.toDouble * (1.0 / 3600) format Seconds
    }

    def variable: Parser[Number] = ident ^^ { x => variables(x) }
    def fNumber: Parser[Number] = floatingPointNumber ^^ { x => x.toDouble }
    def number: Parser[Number] = minutesAndSeconds | minutes | fNumber
    def factor: Parser[Number] = (number | function | variable) | "(" ~> expr <~ ")"

    def operator[T](p: Parser[T], v: => Operator): Parser[Operator] = p ^^^ v

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
