package com.github.opengrabeso.formulafx

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.TextField
import scalafx.scene.layout.BorderPane

object FormulaFX extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title.value = "Formula Fx - Expression Calculator"

    scene = new Scene {
      val pane = new BorderPane {
        val result = new TextField {
        }
        val input = new TextField {
          editable = true

          text.onChange {
            result.text = Evaluate(text.value)
          }

        }
        center = input
        bottom = result

      }

      root = pane

    }
  }
}
