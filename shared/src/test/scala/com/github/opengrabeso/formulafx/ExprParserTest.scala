package com.github.opengrabeso.formulafx

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}
import core._
import Format._
import org.scalactic.{Equivalence, TypeCheckedTripleEquals}

trait ExpressionTestUtils {
  def res(x: Double) = Success(Number(x, General))
}

class ExprParserTest extends AnyFlatSpec with Matchers with TypeCheckedTripleEquals with ExpressionTestUtils {

  implicit val numberEq: Equivalence[Try[Number]] = new Equivalence[Try[Number]] {
    override def areEquivalent(a: Try[Number], b: Try[Number]) = (a, b) match {
      case (Failure(_), Failure(_)) => true
      case (Failure(_), Success(_)) => false
      case (Success(_), Failure(_)) => false
      case (Success(na), Success(nb)) => na.x == nb.x

    }
  }

  behavior of "Expression parser"

  it should "compute simple arithmetic expressions" in {
    Evaluate.ExprParser("1") shouldBe res(1)
    Evaluate.ExprParser("1 + 2") shouldBe res(3)
    Evaluate.ExprParser("1 + 2 * 3") shouldBe res(7)
    Evaluate.ExprParser("(1 + 2) * 3") shouldBe res(9)
  }

  it should "reject invalid expressions" in {
    Evaluate.ExprParser("1+*2") shouldBe a[Failure[_]]
    Evaluate.ExprParser("1+)3") shouldBe a[Failure[_]]
    Evaluate.ExprParser("(1+)3") shouldBe a[Failure[_]]
    Evaluate.ExprParser("(13") shouldBe a[Failure[_]]
  }

  it should "compute expressions with correct associativity" in {
    Evaluate.ExprParser("2-1-1") shouldBe res(0)
    Evaluate.ExprParser("4/2/2") shouldBe res(1)
  }

  it should "parse minutes" in {
    Evaluate.ExprParser("1:30") should === (res(90))
  }

  it should "output minutes as minutes" in {
    Evaluate.ExprParser("1:30+0:10").get.toString shouldBe "1:40"
  }

  it should "output hex as hex" in {
    Evaluate.ExprParser("0x11+0x11").get.toString shouldBe "0x22"
  }

  it should "parse minutes and seconds" in {
    Evaluate.ExprParser("1:30:30") should === (res(3600 + 30*60 + 30))
  }

  it should "compute expressions with functions" in {
    Evaluate.ExprParser("sin(0)") shouldBe res(0)
    Evaluate.ExprParser("cos(0)") shouldBe res(1)
  }

  it should "compute expressions with power" in {
    Evaluate.ExprParser("2 ^ 4") shouldBe res(16)
    Evaluate.ExprParser("2 ^ (1/2)") shouldBe res(Math.sqrt(2))
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

  it should "parse valid hexadecimal numbers" in {
    Evaluate.ExprParser("0x11").get.x shouldBe 0x11
    Evaluate.ExprParser("0XF0").get.x shouldBe 0xf0
    Evaluate.ExprParser("0xff").get.x shouldBe 0xff
    Evaluate.ExprParser("0xA1").get.x shouldBe 0xa1
  }

  it should "reject invalid hexadecimal numbers" in {
    Evaluate.ExprParser("0x1Z") shouldBe a[Failure[_]]
    Evaluate.ExprParser("0x ff") shouldBe a[Failure[_]]
    Evaluate.ExprParser("0xxff") shouldBe a[Failure[_]]
  }
}
