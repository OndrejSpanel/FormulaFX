package com.github.opengrabeso
package formulafx

import org.scalajs.dom._

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import com.github.opengrabeso.js.JsUtils._

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

  def setResult(res: String): Unit = {
    val resultNode = document.getElementById("result")
    resultNode.innerHTML = res
  }


  @JSExport
  def reset(): Unit = {
    tableData.clear()
    Evaluate.clear()

    // remove all rows except the first (headers)
    val tableNode = document.getElementById("history")
    val chs = tableNode.childNodes.copySeq // copy needed to avoid mutation while iterating
    for (n <- chs) {
      val ch = n.firstChild
      // delete all but headers
      if (ch!=null && ch.nodeName=="TD") {
        tableNode.removeChild(n)
      }
    }
    setInput("")
    setResult("")
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

    val resText = Evaluate.compute(str, preview)

    resText.map { res =>
      setResult(res)
      if (!preview) {
        addTableRow(str, "expr")
        addTableRow(resPrefix + res, "result")
        setInput("")
      }
    }

  }

}
