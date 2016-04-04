package com.github.opengrabeso.formulafx

import org.scalatest.{FlatSpec, Matchers}

class EvaluateTest extends FlatSpec with Matchers with ExpressionTestUtils {
  behavior of "Evaluator with variables"

  def createVariables = Map[String, Number]()

  def createEvaluator = new Evaluate.ExprParser()(
    new ExpressionSettings(AngleUnit.Radian, false, createVariables)
  )

  it should "define a variable" in {
    val eval = createEvaluator
    eval.apply("a=123") shouldBe res(123)
    eval.settings.variables("a").x shouldBe 123.0
  }

  it should "define a variable on the right side" in {
    val eval = createEvaluator
    eval.apply("12=a") shouldBe res(12)
    eval.settings.variables("a").x shouldBe 12.0
  }
}
