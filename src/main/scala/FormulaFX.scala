import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.TextField
import scalafx.scene.layout.BorderPane

//noinspection ForwardReference
object FormulaFX extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title.value = "Formula Fx - Expression Calculator"

    scene = new Scene {
      val pane = new BorderPane {
        val input = new TextField {
          editable = true

        }
        val result = new TextField {
          text <== input.text
        }
        center = input
        bottom = result

      }

      root = pane

    }
  }
}
