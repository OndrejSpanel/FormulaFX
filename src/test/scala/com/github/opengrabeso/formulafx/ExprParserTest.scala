package com.github.opengrabeso.formulafx

import org.scalatest.{FlatSpec, Matchers}

class ExprParserTest extends FlatSpec with Matchers {
  "Expression parser" should "compute simple arithmetic expressions" in {
    Evaluate.ExprParser("1") shouldBe 1.0
    Evaluate.ExprParser("1 + 2") shouldBe 3.0
    Evaluate.ExprParser("1 + 2 * 3") shouldBe 7.0
    Evaluate.ExprParser("(1 + 2) * 3") shouldBe 9.0
  }

  it should "compute expressions with functions" in {
    Evaluate.ExprParser("sin(0)") shouldBe 0.0
    Evaluate.ExprParser("cos(0)") shouldBe 1.0
  }
}
