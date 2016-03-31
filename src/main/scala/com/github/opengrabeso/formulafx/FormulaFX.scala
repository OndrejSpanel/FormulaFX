package com.github.opengrabeso.formulafx

import java.util.prefs.Preferences

import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.application.JFXApp
import scalafx.beans.property.StringProperty
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.MenuItem._
import scalafx.scene.control.Menu._
import scalafx.Includes.{function12jfxCallback => _, _}
import scalafx.scene.input._
import scalafx.scene.layout.{BorderPane, VBox}

case class TableRowText(t: String) {
  val text = new StringProperty(this, "text", t)
}

//noinspection ForwardReference
object FormulaFX extends JFXApp {
  def prefs: Preferences = Preferences.userRoot().node(getClass.getPackage.getName.toLowerCase)
  stage = new JFXApp.PrimaryStage {
    title.value = "Formula Fx - Expression Calculator"

    val tableData = ObservableBuffer[TableRowText]()

    def clearTable(): Unit = tableData.clear()

    private def rowId(i: Int) = s"row$i"

    def saveSession(): Unit = {
      val oldSize = prefs.getInt("rows", 0)
      prefs.put("version", "0")
      prefs.putInt("rows", tableData.size)
      tableData.zipWithIndex.foreach {
        case (row,i) =>
          prefs.put(rowId(i), row.text.value)
      }
      for (i <- tableData.size until oldSize) {
        prefs.remove(rowId(i))
      }
    }


    def loadSession(): Unit = {
      val version = prefs.get("version", "")
      if (version.nonEmpty) {
        val rows = prefs.getInt("rows", 0)
        tableData.clear()
        for (i <- 0 until rows) {
          val row = prefs.get(rowId(i), "")
          tableData.add(TableRowText(row))
          // we need to execute even lines so that variables are initialized
          if ((i%2) == 0) Evaluate(row)
        }
      }
    }

    loadSession()

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
            result.text = ""
            saveSession()
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

      val menuBar = new MenuBar {
        useSystemMenuBar = true
        menus = Seq(
          new Menu("File") {
            items = Seq(
              new MenuItem("Clear history") {
                accelerator = new KeyCharacterCombination("N", KeyCombination.ControlDown)
                onAction = handle {clearTable()}
              }
            )
          },
          new Menu("Settings") {
            items = Seq(
              new Menu("Angle unit") {
                items = Seq(
                  new MenuItem("Radian") {
                    accelerator = new KeyCodeCombination(KeyCode.F9)
                    onAction = handle {Evaluate.angleUnitRadian()}
                  },
                  new MenuItem("Degree") {
                    accelerator = new KeyCodeCombination(KeyCode.F10)
                    onAction = handle {Evaluate.angleUnitDegree()}
                  }
                )
              }
            )
          }
        )
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

        top = menuBar
        center = results
        bottom = new VBox(input, result)

      }

      root = pane

    }
  }
}
