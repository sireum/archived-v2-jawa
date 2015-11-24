package org.sireum.jawa.sjc.compile

/**
 * @author fgwei
 */
trait CompileProgress {
  def startUnit(unitPath: String)
  def advance(current: Int, total: Int): Boolean
}