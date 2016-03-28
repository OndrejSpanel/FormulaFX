package com.github.opengrabeso.formulafx

import scalafx.application.Platform

import scalafx.collections.ObservableBuffer
import scalafx.application.JFXApp
import scalafx.beans.property.StringProperty
import scalafx.scene.Scene
import scalafx.scene.control.{TableColumn, TableView, TextField}
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

          columns ++= List(
            new TableColumn[TableRow, String] {
              text = "Expression/Result"
              cellValueFactory = _.value.text
            }
          )
        }

        val input = new TextField {
          editable = true
          Platform.runLater(requestFocus())

          text.onChange {
            val result = Evaluate(text.value)
            tableData.add(new TableRow(result))
            ()
          }

        }
        center = results
        bottom = input

      }

      root = pane

    }
  }
}
