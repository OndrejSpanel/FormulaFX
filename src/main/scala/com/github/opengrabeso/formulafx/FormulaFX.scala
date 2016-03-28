package com.github.opengrabeso.formulafx

import javafx.beans.value.ObservableValue

import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.application.JFXApp
import scalafx.beans.property.StringProperty
import scalafx.scene.Scene
import scalafx.scene.control.{TableView, TextField}
import javafx.scene.control.TableColumn
import javafx.scene.control.TableColumn.CellDataFeatures
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

          columns += new TableColumn[TableRow, String] {
            setText("Expression/Result")
            setCellValueFactory(
              new javafx.util.Callback[CellDataFeatures[TableRow, String], ObservableValue[String]]() {
                override def call(param: CellDataFeatures[TableRow, String]): ObservableValue[String] = {
                  new StringProperty(param.getValue.text)
                }
              }
            )
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
