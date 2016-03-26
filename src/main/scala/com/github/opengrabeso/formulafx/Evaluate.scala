package com.github.opengrabeso.formulafx

import scala.util.parsing.combinator.JavaTokenParsers

object Evaluate {

  object ExprParser extends JavaTokenParsers {
    //def variable  : Parser[Double] = """[a-z]""".r ^^ { x => X(x) }
    def number    : Parser[Double] = floatingPointNumber ^^ { x => x.toDouble }
    def factor    : Parser[Double] = number | "(" ~> expr <~ ")"
    def term      : Parser[Double] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
      case number ~ list => list.foldLeft(number) {
        case (x, "*" ~ y) => x * y
        case (x, "/" ~ y) => x / y
      }
    }
    def expr  : Parser[Double] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
      case number ~ list => list.foldLeft(number) {
        case (x, "+" ~ y) => x + y
        case (x, "-" ~ y) => x - y
      }
    }

    def apply(input: String): Double = parseAll(expr, input) match {
      case Success(result, _) => result
      case failure : NoSuccess => scala.sys.error(failure.msg)
    }
  }

  def apply(input: String): String = {
    ExprParser(input).toString
  }

}
