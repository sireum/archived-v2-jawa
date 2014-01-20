package org.sireum.jawa.alir.interProcedural.pointsToAnalysis

import org.sireum.jawa.Type
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.Instance

final case class PTAInstance(typ : Type, defSite : Context) extends Instance{
  override def clone(newDefSite : Context) : Instance = PTAInstance(typ, newDefSite)
  override def toString : String = "PTAInst(name:" + this.typ + ".defsite:" + this.defSite + ")"
}