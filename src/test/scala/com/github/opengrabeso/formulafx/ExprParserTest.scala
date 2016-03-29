package com.github.opengrabeso.formulafx

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}
import Format._
import org.scalactic.{Equivalence, ConversionCheckedTripleEquals}

class ExprParserTest extends FlatSpec with Matchers with ConversionCheckedTripleEquals {

  def res(x: Double) = Success(Number(x, General))

  implicit val numberEq = new Equivalence[Try[Number]] {
    override def areEquivalent(a: Try[Number], b: Try[Number]) = (a, b) match {
      case (Failure(_), Failure(_)) => true
      case (Failure(_), Success(_)) => false
      case (Success(_), Failure(_)) => false
      case (Success(na), Success(nb)) => na.x == nb.x

    }
  }


  "Expression parser" should "compute simple arithmetic expressions" in {
    Evaluate.ExprParser("1") shouldBe res(1)
    Evaluate.ExprParser("1 + 2") shouldBe res(3)
    Evaluate.ExprParser("1 + 2 * 3") shouldBe res(7)
    Evaluate.ExprParser("(1 + 2) * 3") shouldBe res(9)
  }

  it should "parse minutes" in {
    Evaluate.ExprParser("1:30") should === (res(1.5))
  }

  it should "parse minutes and seconds" in {
    Evaluate.ExprParser("1:30:30") should === (res(1.5 + 30.0/3600))
  }

  it should "compute expressions with functions" in {
    Evaluate.ExprParser("sin(0)") shouldBe res(0)
    Evaluate.ExprParser("cos(0)") shouldBe res(1)
  }

  it should "evaluate assignments" in {
    Evaluate.ExprParser("a = 123") shouldBe res(123.0)
  }

  it should "fail on malformed expressions" in {
    Evaluate.ExprParser("1 + / 2") shouldBe a[Failure[_]]
    Evaluate.ExprParser("1 +") shouldBe a[Failure[_]]
    Evaluate.ExprParser("/ 2") shouldBe a[Failure[_]]
  }

  it should "fail when accessing undefined variable" in {
    Evaluate.ExprParser("unknown + 10") shouldBe a[Failure[_]]
  }

  it should "fail when calling an undefined function" in {
    Evaluate.ExprParser("unknown(10)") shouldBe a[Failure[_]]
  }
}
