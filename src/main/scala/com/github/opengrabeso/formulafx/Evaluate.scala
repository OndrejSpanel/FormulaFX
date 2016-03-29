package com.github.opengrabeso.formulafx

import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

object Evaluate {
  type Number = Double
  type BoxedNumber = java.lang.Double

  var variables = Map[String, Number]()

  object ExprParser extends JavaTokenParsers {
    type Operator = (Number, Number) => Number
    type Function = Number => Number

    def parseFunctionName: Parser[Function] =
      "sin" ^^^ Math.sin _ |
      "cos" ^^^ Math.cos _

    def function: Parser[Number] = parseFunctionName ~ ("(" ~> expr <~ ")") ^^ { case f ~ x => f(x) }

    def minutes: Parser[Number] = (wholeNumber <~ ":") ~ floatingPointNumber ^^ { case deg ~ min => deg.toInt + min.toDouble * (1.0 / 60) }
    def minutesAndSeconds: Parser[Number] = (wholeNumber <~ ":") ~ (wholeNumber <~ ":") ~ floatingPointNumber ^^ {
      case (deg ~ min ~ sec) => deg.toInt + min.toInt * (1.0 / 60) * sec.toDouble * (1.0 / 3600)
    }

    def variable: Parser[Number] = ident ^^ { x => variables(x) }
    def fNumber: Parser[Number] = floatingPointNumber ^^ { x => x.toDouble }
    def number: Parser[Number] = minutesAndSeconds | minutes | fNumber
    def factor: Parser[Number] = (number | function | variable) | "(" ~> expr <~ ")"

    def mulOperators: Parser[Operator] =
      "*" ^^^ ({ _ * _ } : Operator) |
      "/" ^^^ ({ _ / _ } : Operator)

    def addOperators: Parser[Operator] =
      "+" ^^^ ({ _ + _ } : Operator) |
      "-" ^^^ ({ _ - _ } : Operator)

    def term: Parser[Number] = (factor ~ mulOperators ~ term ^^ { case a ~ o ~ b => o(a, b) }) | factor

    def expr: Parser[Number] = (term ~ addOperators ~ expr ^^ { case a ~ o ~ b => o(a, b) }) | term

    def assign: Parser[Number] = (ident <~ "=") ~ expr ^^ {
      case i ~ x =>
        variables += (i -> x)
        x
    }

    def command = assign | expr ^^ { x => x }

    def apply(input: String): Try[Number] = Try {parseAll(command, input)}.map {
      case Success(result, _) => result
      case failure: NoSuccess => throw new UnsupportedOperationException(failure.msg)
    }
  }

  def apply(input: String): Try[String] = {
    ExprParser(input).map(_.toString)
  }

}
