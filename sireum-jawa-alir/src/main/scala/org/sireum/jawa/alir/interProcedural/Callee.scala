package org.sireum.jawa.alir.interProcedural

import org.sireum.jawa.alir.Instance

abstract class Callee {
  def callee : String
}

final case class InstanceCallee(callee : String, ins : Instance) extends Callee

final case class StaticCallee(callee : String) extends Callee