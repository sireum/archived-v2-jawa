package org.sireum.jawa.alir.interProcedural

import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.alir.Instance

abstract class Callee {
  def calleeProc : JawaProcedure
}

final case class InstanceCallee(calleeProc : JawaProcedure, ins : Instance) extends Callee

final case class StaticCallee(calleeProc : JawaProcedure) extends Callee