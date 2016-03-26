import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.TextField
import scalafx.scene.layout.BorderPane

object FormulaFX extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title.value = "Formula Fx - Expression Calculator"

    scene = new Scene {
      val pane = new BorderPane {
        val text = new TextField {
          editable = true
        }
        val result = new TextField
        center = text
        bottom = result
      }

      root = pane

    }
  }
}
