package com.github.opengrabeso.formulafx

import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.application.JFXApp
import scalafx.beans.property.StringProperty
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.MenuItem._
import scalafx.Includes._
import scalafx.scene.control.cell.TextFieldTableCell
import scalafx.scene.layout.BorderPane
import scalafx.util.converter.DefaultStringConverter

case class TableRow(t: String) {
  val text = new StringProperty(this, "text", t)
}

//noinspection ForwardReference
object FormulaFX extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title.value = "Formula Fx - Expression Calculator"


    val tableData = ObservableBuffer[TableRow]()

    scene = new Scene {
      val pane = new BorderPane {
        val results = new TableView[TableRow](tableData) { table =>
          editable = false

          placeholder = new Label("")

          columnResizePolicy = TableView.ConstrainedResizePolicy

          columns += new TableColumn[TableRow, String] {
            maxWidth = Int.MaxValue // http://stackoverflow.com/posts/35265368/edit
            text = "Expression/Result"
            sortable = false
            cellValueFactory = {_.getValue.text}

            //cellFactory = TextFieldTableCell.forTableColumn[TableRow]()
            def factory(x: TableColumn[TableRow, String]) = new TextFieldTableCell[TableRow, String](new DefaultStringConverter())

            cellFactory = factory _

            //cellFactory = _ => new TextFieldTableCell[TableRow, String](new DefaultStringConverter())
          }

          contextMenu = new ContextMenu {
            items += new MenuItem {
              text = "Use"
              onAction = handle {
                val row = table.selectionModel.value.getSelectedItem
                input.text = row.text.value
              }
            }
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
