package org.sireum.jawa.sjc.util

import org.sireum.jawa.sjc.parser.JawaAstNode
import org.sireum.jawa.sjc.parser.JawaSymbol

/** A locator for trees with given positions.
   *  Given a position `pos`, locator.apply returns
   *  the smallest tree that encloses `pos`.
   */
class Locator(pos: Position) {
  var last: JawaAstNode = _
  def locateIn(root: JawaAstNode): JawaAstNode = {
    traverse(root)
    this.last
  }
  def traverse(t: JawaAstNode) {
    t match {
      case tt : JawaSymbol =>
        if (t.pos includes pos) {
          this.last = tt
        }
      case a =>
        if (t.pos includes pos) {
          val children = t.immediateChildren
          children.find(_.pos includes pos) match {
            case Some(c) => traverse(c)
            case None => this.last = a
          }
        }
    }
  }
}