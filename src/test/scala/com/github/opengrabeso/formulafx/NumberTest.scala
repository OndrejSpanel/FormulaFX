package com.github.opengrabeso.formulafx

import org.scalatest.{FlatSpec, Matchers}
import Format._

class NumberTest extends FlatSpec with Matchers {

  behavior of "NumberTest"

  it should "skip empty fraction part" in {
    Number.fractionString(0.0, 5) shouldBe ""
  }

  it should "format fraction part" in {
    Number.fractionString(0.5, 5) shouldBe ".5"
  }

  it should "format fraction part rounded" ignore {
    Number.fractionString(0.999, 2) shouldBe ".5"
  }

  it should "format minutes" in {
    Number(1.5, Minutes).toString shouldBe "1:30"
  }

  it should "format fractional minutes" in {
    Number(1.53, Minutes).toString shouldBe "1:31.8"
  }

  it should "format seconds" in {
    Number(1 + 1.0/3600, Seconds).toString shouldBe "1:00:01"
  }

  it should "format fractional seconds" in {
    Number(1 + 1.11/3600, Seconds).toString shouldBe "1:00:01.11"
  }
}
