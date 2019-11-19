package com.github.opengrabeso.formulafx

import java.awt.event.{InputEvent, KeyEvent}

import core._
import java.util.prefs.Preferences

import javax.swing.text.TableView
import javax.swing.{SwingUtilities, UIManager}

import scala.collection.mutable
import scala.swing.event.{EditDone, ValueChanged}
import scala.swing.{BorderPanel, CheckMenuItem, Dimension, Label, MainFrame, Menu, MenuBar, MenuItem, SimpleSwingApplication, TextField, TextPane}

case class TableRowText(text: String)
//noinspection ForwardReference
object FormulaFX extends SimpleSwingApplication {
  def prefs: Preferences = Preferences.userRoot().node(getClass.getPackage.getName.toLowerCase)

  override def startup(args: scala.Array[scala.Predef.String]): Unit = {

    assert(SwingUtilities.isEventDispatchThread)

    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

    super.startup(args)
  }

  def top: MainFrame = new MainFrame {
    title = "Formula Fx - Expression Calculator"

    minimumSize = new Dimension(800, 600)

    //private val icon = new Image("/calculator.png")
    //icons.add(icon)

    val tableData = mutable.ArrayBuffer.empty[TableRowText]

    def clearTable(): Unit = tableData.clear()

    private def rowId(i: Int) = s"row$i"

    def saveSession(): Unit = {
      val oldSize = prefs.getInt("rows", 0)
      prefs.put("version", "0")
      prefs.putInt("rows", tableData.size)
      tableData.zipWithIndex.foreach {
        case (row,i) =>
          prefs.put(rowId(i), row.text)
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
          tableData += TableRowText(row)
          // we need to execute even lines so that variables are initialized
          if ((i%2) == 0) Evaluate.compute(row, false)
        }
      }
    }

    loadSession()


    val result = new TextField {
      editable = false
    }
    val input = new TextField {
      editable = true
    }

    def computeResult(preview: Boolean): Unit = {
      val resultText = Evaluate.compute(input.text, preview)
      resultText.map { res =>
        result.text = res
      }
    }

    listenTo(input)
    reactions += {
      case EditDone(`input`) =>
        val resultText = Evaluate.compute(input.text, false)
        resultText.map { res =>
          tableData += TableRowText(input.text)
          tableData += TableRowText("  " + res)
          input.text = ""
          result.text = ""
          saveSession()
        }
      case ValueChanged(`input`) =>
        computeResult(true)
    }
    val statusBar = new Label

    private val menuRadian = new CheckMenuItem("Radian") {
      //accelerator = new KeyCodeCombination(KeyCode.F9)
      //onAction = handle {changeSettings{Evaluate.angleUnitRadian()}}
    }
    private val menuDegree = new CheckMenuItem("Degree") {
      //accelerator = new KeyCodeCombination(KeyCode.F10)
      //onAction = handle {changeSettings{Evaluate.angleUnitDegree()}}
    }
    def changeSettings(change: => Unit): Unit = {
      change
      showStatus()
      computeResult(true)

    }
    val menu = new MenuBar {
      contents += new Menu("Settings") {
        contents += new Menu("Angle unit") {
          contents += menuRadian
          contents += menuDegree
        }
      }

      contents +=  new Menu("File") {
        contents += new MenuItem(new ActionWithCode("Clear history", {clearTable(); Evaluate.clear()}, KeyEvent.VK_N, InputEvent.CTRL_DOWN_MASK))
      }
    }

    menuBar = menu

    def showStatus(): Unit = {
      statusBar.text = Evaluate.angleUnitName
      menuDegree.selected = Evaluate.angleUnitIsDegree
      menuRadian.selected = Evaluate.angleUnitIsRadian
    }

    val pane = new BorderPanel {
      import BorderPanel.Position._

      /*
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

      add(results, Center)
      center = results

       */
      add(input, Center)
      add(result, South)
      // TODO: statusBar
    }

    contents = pane
    showStatus()

  }
}
