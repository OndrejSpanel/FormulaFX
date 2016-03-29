package com.github.opengrabeso.formulafx

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class ExprParserTest extends FlatSpec with Matchers {
  "Expression parser" should "compute simple arithmetic expressions" in {
    Evaluate.ExprParser("1") shouldBe Success(1.0)
    Evaluate.ExprParser("1 + 2") shouldBe Success(3.0)
    Evaluate.ExprParser("1 + 2 * 3") shouldBe Success(7.0)
    Evaluate.ExprParser("(1 + 2) * 3") shouldBe Success(9.0)
  }

  it should "compute expressions with functions" in {
    Evaluate.ExprParser("sin(0)") shouldBe Success(0.0)
    Evaluate.ExprParser("cos(0)") shouldBe Success(1.0)
  }

  it should "evaluate assignments with functions" in {
    Evaluate.ExprParser("a = 123") shouldBe Success(123.0)
  }
}
