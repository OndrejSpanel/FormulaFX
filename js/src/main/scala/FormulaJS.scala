package com.github.opengrabeso.formulafx

import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom.document

import scala.scalajs.js.annotation.JSExport

/**
  * Created by Ondra on 9.5.2016.
  */
object FormulaJS extends JSApp {
  def main(): Unit = {
    println("Formula JS")
  }

  @JSExport
  def eval(str: String): Unit = {
    val resultNode = document.getElementById("result")
    val resText = Evaluate.compute(str, false)

    resText.map { res =>
      resultNode.innerHTML = res
      // TODO: clear input field
    }

  }
}
