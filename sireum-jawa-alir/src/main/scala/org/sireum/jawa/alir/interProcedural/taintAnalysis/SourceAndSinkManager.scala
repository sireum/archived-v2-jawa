package org.sireum.jawa.alir.interProcedural.taintAnalysis

import org.sireum.jawa.JawaProcedure
import org.sireum.pilar.ast.JumpLocation

trait SourceAndSinkManager {
  def isSource(calleeProcedure : JawaProcedure, callerProcedure : JawaProcedure, callerLoc : JumpLocation) : Boolean
  def isSourceProcedure(procedure : JawaProcedure) : Boolean
  def isSinkProcedure(procedure : JawaProcedure) : Boolean
}