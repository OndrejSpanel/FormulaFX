package com.github.opengrabeso.formulafx

import org.scalatest.{FlatSpec, Matchers}
import Format._

class NumberTest extends FlatSpec with Matchers {

  behavior of "NumberTest"

  it should "format minutes" in {
    Number(1.5, Minutes).toString shouldBe "1:30"

  }

}
