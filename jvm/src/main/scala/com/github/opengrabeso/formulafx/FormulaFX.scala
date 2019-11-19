package com.github.opengrabeso.formulafx

import java.awt.Color
import java.awt.event.{InputEvent, KeyEvent}

import core._
import java.util.prefs.Preferences

import javax.swing.table.{DefaultTableCellRenderer, DefaultTableModel, TableCellRenderer}
import javax.swing.{JTable, SwingUtilities, UIManager}

import scala.collection.mutable
import scala.swing.event._
import scala.swing._

object FormulaFX extends SimpleSwingApplication {
  case class TableRowText(text: String)

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
    val model: DefaultTableModel = new DefaultTableModel(Array[AnyRef]("Expression / result"), 0) {
      override def isCellEditable(row: Int, column: Int) = false
    }

    def clearTable(): Unit = {
      tableData.clear()
      while (model.getRowCount > 0) {
        model.removeRow(0)
      }
    }

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



    val stripedRenderer: TableCellRenderer = new TableCellRenderer {
      private val defRenderer = new DefaultTableCellRenderer
      private val stripeRenderer: DefaultTableCellRenderer = new DefaultTableCellRenderer {
        override def setBackground(c: Color) = {
          val color = UIManager.getColor ( "Panel.background" )
          super.setBackground(color.darker)
        }
      }
      def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int) = {
        val r = if ( (row % 2) == 0) defRenderer else stripeRenderer
        r.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
      }
    }

    val table  = new Table(model) {
      selection.elementMode = Table.ElementMode.Row
      peer.setDefaultRenderer(classOf[String], stripedRenderer)
    }


    def addRow(row: String): Unit = {
      tableData += TableRowText(row)
      table.model.asInstanceOf[DefaultTableModel].addRow(Array[AnyRef](row))
    }

    loadSession()
    for (r <- tableData) {
      table.model.asInstanceOf[DefaultTableModel].addRow(Array[AnyRef](r.text))
    }


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
          addRow(input.text)
          addRow(" " + res)
          input.text = ""
          result.text = ""
          saveSession()
        }
      case ValueChanged(`input`) =>
        computeResult(true)
    }
    val statusBar = new Label

    private val menuRadian = new RadioMenuItem("Radian") { item =>
      //accelerator = new KeyCodeCombination(KeyCode.F9)
      //onAction = handle {changeSettings{Evaluate.angleUnitRadian()}}
    }
    private val menuDegree = new RadioMenuItem("Degree") {
      //accelerator = new KeyCodeCombination(KeyCode.F10)
      //onAction = handle {changeSettings{Evaluate.angleUnitDegree()}}
    }
    listenTo(menuRadian, menuDegree)
    reactions += {
      case ButtonClicked(`menuRadian`) | ButtonClicked(`menuDegree`) =>
        if (menuRadian.selected) changeSettings(Evaluate.angleUnitRadian())
        else changeSettings(Evaluate.angleUnitDegree())
    }
    def changeSettings(change: => Unit): Unit = {
      change
      showStatus()
      computeResult(true)

    }
    val menu = new MenuBar {
      contents += new Menu("Settings") {
        contents += new Menu("Angle unit") {
          new ButtonGroup(menuRadian, menuDegree).buttons.foreach(contents += _)
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

    val pane: BorderPanel = new BorderPanel {
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
      add(new ScrollPane(table), Center)

      val bottom = new BoxPanel(Orientation.Vertical) {
        contents += input
        contents += result
        contents += statusBar
      }
      add(bottom, South)
      // TODO: statusBar
    }

    contents = pane
    showStatus()

  }
}
