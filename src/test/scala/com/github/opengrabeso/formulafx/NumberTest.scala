package com.github.opengrabeso.formulafx

import org.scalatest.{FlatSpec, Matchers}
import Format._

class NumberTest extends FlatSpec with Matchers {

  behavior of "NumberTest"

  it should "skip empty fraction part" in {
    Number.fractionString(0.0, 5).toString shouldBe ""
  }

  it should "format fraction part" in {
    Number.fractionString(0.5, 5).toString shouldBe ".5"
    Number.fractionString(0.05, 5).toString shouldBe ".05"
  }

  it should "format fraction part rounded" in {
    Number.fractionString(0.222, 2).toString shouldBe ".22"
    Number.fractionString(0.555, 2).toString shouldBe ".56"
  }

  it should "format minutes" in {
    Number(1.5, Minutes).toString shouldBe "1:30"
  }

  it should "handle huge time gracefully" in {
    Number(1e10, Minutes).toString shouldBe 1e10.toString
    Number(1e10, Seconds).toString shouldBe 1e10.toString
  }

  it should "format fractional minutes" in {
    Number(1.53, Minutes).toString shouldBe "1:31.8"
  }

  it should "format fractional minutes rounded with overflow" in {
    Number(1.999999999, Minutes).toString shouldBe "2:00"
  }

  it should "format fractional seconds rounded with overflow" in {
    Number(1.999999999, Seconds).toString shouldBe "2:00:00"
  }

  it should "format seconds" in {
    Number(1 + 1.0/3600, Seconds).toString shouldBe "1:00:01"
  }

  it should "format fractional seconds" in {
    Number(1 + 1.11/3600, Seconds).toString shouldBe "1:00:01.11"
  }
}
