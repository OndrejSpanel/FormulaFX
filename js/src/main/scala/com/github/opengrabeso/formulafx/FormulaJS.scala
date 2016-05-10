package com.github.opengrabeso.formulafx

import org.scalajs.dom._

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

/**
  * Created by Ondra on 9.5.2016.
  */
object FormulaJS extends JSApp {
  def main(): Unit = {
    println("Formula JS")
  }

  val tableData = mutable.ArrayBuffer[String]()

  def addTableRow(str: String, c: String): Unit = {
    tableData += str
    val tableNode = document.getElementById("history")
    val tr = document.createElement("tr")
    val td = document.createElement("td")
    td.setAttribute("class", c)
    tr.appendChild(td)
    td.innerHTML = str
    tableNode.appendChild(tr)
  }

  @JSExport
  def eval(str: String, preview: Boolean): Unit = {
    val document = js.Dynamic.global.document // evalNode.value not working without Dynamic
    val resultNode = document.getElementById("result")
    val evalNode = document.getElementById("eval") //.asInstanceOf[html.Paragraph]

    val resText = Evaluate.compute(str, preview)

    resText.map { res =>
      resultNode.innerHTML = res
      if (!preview) {
        addTableRow(str, "expr")
        addTableRow("&nbsp;&nbsp;" + res, "result")
        evalNode.value = ""
      }
    }

  }
}
