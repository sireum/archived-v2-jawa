package org.sireum.jawa

/**
 * @author fgwei
 */
case class FatalError(msg: String) extends Exception(msg)

case class InheritanceError(msg: String) extends Exception(msg)