package com.github.opengrabeso.formulafx

import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.application.JFXApp
import scalafx.beans.property.StringProperty
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.MenuItem._
import scalafx.Includes.{function12jfxCallback => _, _}
import scalafx.scene.input.{MouseButton, MouseEvent}
import scalafx.scene.layout.{BorderPane, VBox}

case class TableRowText(t: String) {
  val text = new StringProperty(this, "text", t)
}

//noinspection ForwardReference
object FormulaFX extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title.value = "Formula Fx - Expression Calculator"


    val tableData = ObservableBuffer[TableRowText]()

    scene = new Scene {
      val result = new TextField {
        editable = false
      }
      val input = new TextField {
        editable = true
        Platform.runLater(requestFocus())

        onAction = handle {
          val resultText = Evaluate(text.value)
          resultText.map { res =>
            tableData.add(new TableRowText(text.value))
            tableData.add(new TableRowText("  " + res))
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

      val pane = new BorderPane {

        val results = new TableView[TableRowText](tableData) { table =>
          editable = false

          placeholder = new Label("")

          columnResizePolicy = TableView.ConstrainedResizePolicy

          def useSelectedRow(): Unit = {
            val row = table.selectionModel.value.getSelectedItem
            input.text = row.text.value
          }

          columns += new TableColumn[TableRowText, String] {
            maxWidth = Int.MaxValue // http://stackoverflow.com/posts/35265368/edit
            text = "Expression/Result"
            sortable = false
            cellValueFactory = {_.getValue.text}
          }

          rowFactory = t => new TableRow[TableRowText] {
            onMouseClicked = (me: MouseEvent) => {
              if (me.button == MouseButton.PRIMARY && me.clickCount == 2) {
                useSelectedRow()
              }
            }
          }

          contextMenu = new ContextMenu {
            items += new MenuItem {
              text = "Use"
              onAction = handle {
                useSelectedRow()
              }
            }
          }
        }

        center = results
        bottom = new VBox(input, result)

      }

      root = pane

    }
  }
}
