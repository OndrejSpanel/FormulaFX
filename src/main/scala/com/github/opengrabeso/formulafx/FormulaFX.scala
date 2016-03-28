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
                  param.getValue.text
                }
              }
            )
          }
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
