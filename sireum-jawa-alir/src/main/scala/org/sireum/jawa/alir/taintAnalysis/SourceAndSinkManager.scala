package org.sireum.jawa.alir.taintAnalysis

import org.sireum.jawa.JawaProcedure
import org.sireum.pilar.ast.JumpLocation
import org.sireum.pilar.ast.LocationDecl
import org.sireum.util.ISet
import org.sireum.jawa.alir.reachingFactsAnalysis.RFAFact

trait SourceAndSinkManager {
  def isSource(loc : LocationDecl, s : ISet[RFAFact]) : Boolean
  def isSource(calleeProcedure : JawaProcedure, callerProcedure : JawaProcedure, callerLoc : JumpLocation) : Boolean
  def isSourceProcedure(procedure : JawaProcedure) : Boolean
  def isSink(loc : LocationDecl, s : ISet[RFAFact]) : Boolean
  def isSinkProcedure(procedure : JawaProcedure) : Boolean
}