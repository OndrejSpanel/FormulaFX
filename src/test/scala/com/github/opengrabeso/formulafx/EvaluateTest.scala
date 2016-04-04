package com.github.opengrabeso.formulafx

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Failure

class EvaluateTest extends FlatSpec with Matchers with ExpressionTestUtils {
  behavior of "Evaluator with variables"

  def createVariables = collection.mutable.Map[String, Number]()

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

  private def computeEquation(eq: String) = {
    val eval = createEvaluator
    eval(eq)
  }
  private def testEquation(eq: String, unknown: String, result: Double): Unit = {
    val eval = createEvaluator
    eval(eq)
    eval.settings.variables(unknown).x shouldBe result
  }

  it should "solve simple equations" in {
    testEquation("1+2 = a*3", "a", 1)
    testEquation("1+2 = 12/a", "a", 4)
    testEquation("1+2 = 12/(6-a)", "a", 2)
    testEquation("1+2 = 12/(a+1)", "a", 3)
    testEquation("1+2 = a-2", "a", 5)
    testEquation("1+2 = a/2", "a", 6)
    testEquation("(1+2)*3 = 6/((2/a)/3)", "a", 1)
  }

  it should "solve equations with formatted numbers" in {
    testEquation("a * 0x11 = 0x88", "a", 8)
  }

  it should "invert functions" in {
    testEquation("sqrt(a) = 2", "a", 4)
    testEquation("cos(a) = 1", "a", 0)
  }

  it should "not invert non-inversible functions" in {
    computeEquation("round(x) = 0") shouldBe a[Failure[_]]
    computeEquation("ceil(x) = 0") shouldBe a[Failure[_]]
  }
}
