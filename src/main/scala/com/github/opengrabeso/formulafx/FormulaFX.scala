package com.github.opengrabeso.formulafx

import javafx.beans.value.ObservableValue

import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.application.JFXApp
import scalafx.beans.property.StringProperty
import scalafx.scene.Scene
import scalafx.scene.control.{Label, TableView, TextField}
import javafx.scene.control.TableColumn

import scalafx.Includes._
import scalafx.scene.layout.BorderPane

object FormulaFX extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title.value = "Formula Fx - Expression Calculator"

    case class TableRow(t: String) {
      val text = new StringProperty(this, "text", t)
    }

    val tableData = ObservableBuffer[TableRow]()

    scene = new Scene {
      val pane = new BorderPane {
        val results = new TableView[TableRow](tableData) {
          editable = false

          placeholder = new Label("")

          columns += new TableColumn[TableRow, String] {
            setText("Expression/Result")
            val cb: (TableColumn.CellDataFeatures[TableRow, String] => ObservableValue[String]) = _.getValue.text
            setCellValueFactory(cb)
          }
        }

        val result = new TextField {
          editable = false
        }
        val input = new TextField {
          editable = true
          Platform.runLater(requestFocus())

          onAction = handle {
            val resultText = Evaluate(text.value)
            tableData.add(new TableRow(resultText))
            text = ""
          }

          text.onChange {
            val resultText = Evaluate(text.value)
            result.text = resultText
            ()
          }

        }
        top = results
        center = input
        bottom = result

      }

      root = pane

    }
  }
}
