package com.github.opengrabeso
package formulafx

import core._

import org.scalajs.dom._

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import com.github.opengrabeso.js.JsUtils._
import org.scalajs.dom

@JSExport(name="FormulaJS")
object FormulaJS extends JSApp {

  private val resPrefix = "&nbsp;&nbsp;"

  private val prefs = dom.window.localStorage

  import com.github.opengrabeso.js.Prefs._


  def main(): Unit = {
    println("Formula JS")
  }

  val tableData = mutable.ArrayBuffer[String]()

  def addTableRow(str: String, c: String): Unit = {
    tableData += str
    val tableNode = document.getElementById("history")
    val tr = document.createElement("tr")
    val tdr = document.createElement("td")
    tr.appendChild(tdr)
    tdr.setAttribute("class", c)

    val td = document.createElement("td")
    td.innerHTML = str
    tdr.appendChild(td)

    /*
    val tdPin = document.createElement("td")
    tdPin.setAttribute("class","pin")
    tdPin.innerHTML = ">"
    tdr.appendChild(tdPin)
    */

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

  private def rowId(i: Int) = s"row$i"

  def saveSession(): Unit = {

    val oldSize = prefs.getInt("rows", 0)
    prefs.put("version", "0")
    prefs.putInt("rows", tableData.size)
    tableData.zipWithIndex.foreach {
      case (row, i) =>
        prefs.put(rowId(i), row)
    }
    for (i <- tableData.size until oldSize) {
      prefs.remove(rowId(i))
    }
  }

  @JSExport
  def loadSession(): Unit = {
    clearTable()
    val version = prefs.get("version", "")
    if (version.nonEmpty) {
      val rows = prefs.getInt("rows", 0)
      for (i <- 0 until rows) {
        val row = prefs.get(rowId(i), "")
        // we need to execute even lines so that variables are initialized
        if ((i % 2) == 0) {
          Evaluate.compute(row, false)
          addTableRow(row, "expr")
        } else {
          addTableRow(row, "result")
        }
      }
    }
  }


  @JSExport
  def reset(): Unit = {
    Evaluate.clear()

    clearTable()
    setInput("")
    setResult("")
  }

  def clearTable(): Unit = {
    tableData.clear()
    // remove all rows except the first (headers)
    val tableNode = document.getElementById("history")
    val chs = tableNode.childNodes.copySeq // copy needed to avoid mutation while iterating
    for (n <- chs) {
      val ch = n.firstChild
      // delete all but headers
      if (ch != null && ch.nodeName == "TD") {
        tableNode.removeChild(n)
      }
    }
  }

  @JSExport
  def tableClicked(element: Element): Unit = {
    if (element.nodeName == "TD") {
      def leafCell(e: Element): Element = {
        val ch = e.firstElementChild
        if (ch != null) leafCell(ch)
        else e
      }
      val cell = leafCell(element)
      val cellText = cell.innerHTML
      val resultNode = document.getElementById("result")
      setInput(cellText)
      eval(cellText, true)
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
        saveSession()
      }
    }

  }

}
