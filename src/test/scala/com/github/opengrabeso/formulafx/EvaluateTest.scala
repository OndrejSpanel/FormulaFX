package com.github.opengrabeso.formulafx

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Failure

class EvaluateTest extends FlatSpec with Matchers with ExpressionTestUtils {
  behavior of "Evaluator with variables"

  def createVariables = Map[String, Number]()

  def createEvaluator = new Evaluate.ExprParser()(
    new ExpressionSettings(AngleUnit.Radian, false, createVariables)
  )

  it should "define a variable" in {
    val eval = createEvaluator
    eval("a=123") shouldBe res(123)
    eval.settings.variables("a").x shouldBe 123.0
  }

  it should "define a variable on the right side" in {
    val eval = createEvaluator
    eval("12=a") shouldBe res(12)
    eval.settings.variables("a").x shouldBe 12.0
    eval("a=z") shouldBe res(12)
    eval.settings.variables("z").x shouldBe 12.0
  }

  it should "redefine leftmost variable" in {
    val eval = createEvaluator
    eval("12=a") shouldBe res(12)
    eval("a=456") shouldBe res(456)
    eval.settings.variables("a").x shouldBe 456.0
  }

  it should "reject equation with multiple variables" in {
    val eval = createEvaluator
    eval("a=b") shouldBe a[Failure[_]]
  }
}
