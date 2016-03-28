package com.github.opengrabeso.formulafx

import org.scalatest.{FlatSpec, Matchers}

class ExprParserTest extends FlatSpec with Matchers {
  "Expression parser" should "compute simple expressions" in {
    Evaluate.ExprParser("1") shouldBe 1.0
    Evaluate.ExprParser("1 + 2") shouldBe 3.0
    Evaluate.ExprParser("1 + 2 * 3") shouldBe 7.0
    Evaluate.ExprParser("(1 + 2) * 3") shouldBe 9.0
  }
}
