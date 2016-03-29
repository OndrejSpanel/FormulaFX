package com.github.opengrabeso.formulafx

import com.github.opengrabeso.formulafx.Evaluate.Number
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}
import Evaluate.Format._

class ExprParserTest extends FlatSpec with Matchers {

  def res(x: Double) = Success(Number(x, General))

  "Expression parser" should "compute simple arithmetic expressions" in {
    Evaluate.ExprParser("1") shouldBe res(1)
    Evaluate.ExprParser("1 + 2") shouldBe res(3)
    Evaluate.ExprParser("1 + 2 * 3") shouldBe res(7)
    Evaluate.ExprParser("(1 + 2) * 3") shouldBe res(9)
  }

  it should "compute expressions with functions" in {
    Evaluate.ExprParser("sin(0)") shouldBe res(0)
    Evaluate.ExprParser("cos(0)") shouldBe res(1)
  }

  it should "evaluate assignments with functions" in {
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
