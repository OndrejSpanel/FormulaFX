package com.github.opengrabeso.formulafx

import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.application.JFXApp
import scalafx.beans.property.StringProperty
import scalafx.scene.Scene
import scalafx.scene.control.{Label, TableView, TextField}
import scalafx.scene.control.TableColumn
import scalafx.scene.control.TableColumn._
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

          columnResizePolicy = TableView.ConstrainedResizePolicy

          columns += new TableColumn[TableRow, String] {
            maxWidth = Int.MaxValue // http://stackoverflow.com/posts/35265368/edit
            text = "Expression/Result"
            cellValueFactory = {_.getValue.text}
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
            resultText.map { res =>
              tableData.add(new TableRow(text.value))
              tableData.add(new TableRow("  " + res))
              text = ""
            }
          }

          text.onChange {
            val resultText = Evaluate(text.value)
            resultText.map { res =>
              result.text = res
            }
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
