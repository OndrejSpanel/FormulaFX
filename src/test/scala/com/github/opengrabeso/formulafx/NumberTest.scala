package com.github.opengrabeso.formulafx

import org.scalatest.{FlatSpec, Matchers}
import core._
import Format._

class NumberTest extends FlatSpec with Matchers {

  behavior of "NumberTest"

  def timeMS(deg: Int, min: Double) = deg * 60 + min
  def timeHMS(deg: Int, min: Int, sec: Double) = deg * 3600 + min * 60 + sec

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
    Number(90, Minutes).toString shouldBe "1:30"
  }

  it should "format hours" in {
    Number(3600 + 90, Hours).toString shouldBe "1:01:30"
  }

  it should "handle huge time gracefully" in {
    Number(1e20, Minutes).toString shouldBe 1e20.toString
    Number(1e20, Hours).toString shouldBe 1e20.toString
  }

  it should "format fractional minutes" in {
    Number(91.8, Minutes).toString shouldBe "1:31.8"
  }

  it should "format fractional minutes rounded with overflow" in {
    Number(119.999999999, Minutes).toString shouldBe "2:00"
    Number(timeMS(1, 0.999999), Minutes).toString shouldBe "1:01"
  }

  it should "format fractional seconds rounded with overflow" in {
    Number(1.999999999 * 3600, Hours).toString shouldBe "2:00:00"
    Number(timeHMS(1, 12, 59.999999), Hours).toString shouldBe "1:13:00"
    Number(timeHMS(1, 12, 0.999999), Hours).toString shouldBe "1:12:01"
  }

  it should "format seconds" in {
    Number(3600 + 1, Hours).toString shouldBe "1:00:01"
  }

  it should "format fractional seconds" in {
    Number(3600 + 1.11, Hours).toString shouldBe "1:00:01.11"
  }
}
