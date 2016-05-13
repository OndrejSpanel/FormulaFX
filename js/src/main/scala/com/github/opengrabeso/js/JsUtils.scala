package com.github.opengrabeso.js

import org.scalajs.dom.{DOMList, Node}

object JsUtils {
  implicit class NodeListOps[T <: Node](nodes: DOMList[T])  {
    def copySeq: IndexedSeq[T] = for (n <- nodes) yield n
  }
  implicit class NodeListSeq[T <: Node](nodes: DOMList[T]) extends IndexedSeq[T] {
    override def foreach[U](f: T => U): Unit = {
      for (i <- 0 until nodes.length) {
        f(nodes(i))
      }
    }

    override def length: Int = nodes.length

    override def apply(idx: Int): T = nodes(idx)
  }
}
