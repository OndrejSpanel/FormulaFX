package com.github.opengrabeso.formulafx

import scala.util.parsing.combinator.JavaTokenParsers

object Evaluate {
  type Number = Double

  var variables = Map[String, Number]()

  object ExprParser extends JavaTokenParsers {

    type Operator = (Number, Number) => Number

    def minutes: Parser[Number] = (wholeNumber <~ ":") ~ floatingPointNumber ^^ { case deg ~ min => deg.toInt + min.toDouble * (1.0 / 60) }
    def minutesAndSeconds: Parser[Number] = (wholeNumber <~ ":") ~ (wholeNumber <~ ":") ~ floatingPointNumber ^^ {
      case (deg ~ min ~ sec) => deg.toInt + min.toInt * (1.0 / 60) * sec.toDouble * (1.0 / 3600)
    }

    def variable: Parser[Number] = ident ^^ { x => variables.getOrElse(x, 0.0) }
    def fNumber: Parser[Number] = floatingPointNumber ^^ { x => x.toDouble }
    def number: Parser[Number] = minutesAndSeconds | minutes | fNumber
    def factor: Parser[Number] = (number | variable) | "(" ~> expr <~ ")"

    def op_* : Parser[Operator] = "*" ^^^ { (a, b) => a * b }
    def op_/ : Parser[Operator] = "/" ^^^ { (a, b) => a / b }
    def op_+ : Parser[Operator] = "+" ^^^ { (a, b) => a + b }
    def op_- : Parser[Operator] = "-" ^^^ { (a, b) => a - b }

    def mulOperators: Parser[Operator] = op_* | op_/
    def addOperators: Parser[Operator] = op_+ | op_-

    def term: Parser[Number] = factor ~ rep(mulOperators ~ factor) ^^ {
      case number ~ list => list.foldLeft(number) { case (x, op ~ y) => op(x, y) }
    }

    def expr: Parser[Number] = term ~ rep(addOperators ~ term) ^^ {
      case number ~ list => list.foldLeft(number) { case (x, op ~ y) => op(x, y) }
    }

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
