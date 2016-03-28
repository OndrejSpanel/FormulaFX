package com.github.opengrabeso.formulafx

import scala.util.parsing.combinator.JavaTokenParsers

object Evaluate {
  type Number = Double
  type BoxedNumber = java.lang.Double

  var variables = Map[String, Number]()

  object ExprParser extends JavaTokenParsers {
    type Operator = (Number, Number) => Number
    type Function = Number => Number

    def f_sin: Parser[Function] = "sin" ^^^ Math.sin
    def f_cos: Parser[Function] = "cos" ^^^ Math.cos

    def parseFunctionName: Parser[Function] = f_sin | f_cos

    def function: Parser[Number] = parseFunctionName ~ ("(" ~> expr <~ ")") ^^ { case f ~ x => f(x) }

    def minutes: Parser[Number] = (wholeNumber <~ ":") ~ floatingPointNumber ^^ { case deg ~ min => deg.toInt + min.toDouble * (1.0 / 60) }
    def minutesAndSeconds: Parser[Number] = (wholeNumber <~ ":") ~ (wholeNumber <~ ":") ~ floatingPointNumber ^^ {
      case (deg ~ min ~ sec) => deg.toInt + min.toInt * (1.0 / 60) * sec.toDouble * (1.0 / 3600)
    }

    def variable: Parser[Number] = ident ^^ { x => variables.getOrElse(x, 0.0) }
    def fNumber: Parser[Number] = floatingPointNumber ^^ { x => x.toDouble }
    def number: Parser[Number] = minutesAndSeconds | minutes | fNumber
    def factor: Parser[Number] = (number | function | variable) | "(" ~> expr <~ ")"

    def op_* : Parser[Operator] = "*" ^^^ { _ * _ }
    def op_/ : Parser[Operator] = "/" ^^^ { _ / _ }
    def op_+ : Parser[Operator] = "+" ^^^ { _ + _ }
    def op_- : Parser[Operator] = "-" ^^^ { _ - _ }

    def mulOperators: Parser[Operator] = op_* | op_/
    def addOperators: Parser[Operator] = op_+ | op_-

    def term: Parser[Number] = (factor ~ mulOperators ~ term ^^ { case a ~ o ~ b => o(a, b) }) | factor

    def expr: Parser[Number] = (term ~ addOperators ~ expr ^^ { case a ~ o ~ b => o(a, b) }) | term

    def assign: Parser[Number] = (ident <~ "=") ~ expr ^^ {
      case i ~ x =>
        variables += (i -> x)
        x
    }

    def command = assign | expr ^^ { x => x }

    def apply(input: String): Double = parseAll(command, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => 0
    }
  }

  def apply(input: String): String = {
    ExprParser(input).toString
  }

}
