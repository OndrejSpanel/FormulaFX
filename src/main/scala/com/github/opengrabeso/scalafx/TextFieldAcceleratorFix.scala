package com.github.opengrabeso.scalafx

import scalafx.scene.control.TextField
import scalafx.Includes._
import scalafx.scene.input.{KeyCodeCombination, KeyEvent}

class TextFieldAcceleratorFix extends TextField {
  onKeyReleased = { e: KeyEvent =>
    val toRun = Option(scene.value.accelerators(new KeyCodeCombination(e.code)))
    toRun.foreach(_.run())
  }
}
