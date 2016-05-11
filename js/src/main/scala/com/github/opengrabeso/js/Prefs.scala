package com.github.opengrabeso.js

import org.scalajs.dom._

object Prefs {
  implicit class ToIntDef(s: String) {
    def toIntDef(d: Int): Int = if (s == null) d else s.toInt
    def toStringDef(d: String): String = if (s == null) d else s
  }
  implicit class Prefs(store: Storage) {
    def get(name: String, d: String) = store.getItem(name).toStringDef(d)
    def getInt(name: String, d: Int) = store.getItem(name).toIntDef(d)

    def put(name: String, v: String) = store.setItem(name, v)
    def putInt(name: String, v: Int) = store.setItem(name, v.toString)

    def remove(name: String) = store.removeItem(name)
  }

}
