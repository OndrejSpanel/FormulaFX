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

  private val resPrefix = "&nbsp;&nbsp;"

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

  def setInput(str: String): Unit = {
    val document = js.Dynamic.global.document // evalNode.value not working without Dynamic
    val evalNode = document.getElementById("eval") //.asInstanceOf[html.Paragraph]
    if (str.startsWith(resPrefix)) {
      evalNode.value = str.drop(resPrefix.length)
    } else {
      evalNode.value = str
    }
  }

  @JSExport
  def tableClicked(element: Element): Unit = {
    if (element.nodeName=="TD") {
      val resultNode = document.getElementById("result")
      setInput(element.innerHTML)
      eval(element.innerHTML, true)
    }
  }

  @JSExport
  def eval(str: String, preview: Boolean): Unit = {
    val resultNode = document.getElementById("result")

    val resText = Evaluate.compute(str, preview)

    resText.map { res =>
      resultNode.innerHTML = res
      if (!preview) {
        addTableRow(str, "expr")
        addTableRow(resPrefix + res, "result")
        setInput("")
      }
    }

  }
}
