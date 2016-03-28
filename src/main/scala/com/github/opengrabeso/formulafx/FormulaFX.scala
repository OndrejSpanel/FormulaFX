package com.github.opengrabeso.formulafx

import javafx.application.Platform

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
          editable = false
        }
        val input = new TextField {
          editable = true
          Platform.runLater(new Runnable {def run() = requestFocus()})

          text.onChange {
            result.text = Evaluate(text.value)
          }

        }
        center = result
        bottom = input

      }

      root = pane

    }
  }
}
