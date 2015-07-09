package org.sireum.jawa.alir.pta.reachingFactsAnalysis.model

import org.sireum.jawa.JawaMethod
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.reachingFactsAnalysis.RFAFact
import org.sireum.jawa.alir.Context
import org.sireum.util._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.ScopeManager

object ConstructorModel {
  val TITLE = "ConstructorModel"
  def isConstructor(m: JawaMethod): Boolean = {
    val res = ScopeManager.getCurrentScopeManager.shouldBypass(m.getDeclaringClass) &&
    m.getName.contains(m.getDeclaringClass.constructorName)
    res
  }
  
  def doConstructorCall(s: PTAResult, p: JawaMethod, args: List[String], retVars: Seq[String], currentContext: Context): (ISet[RFAFact], ISet[RFAFact], Boolean) = {
    var newFacts = isetEmpty[RFAFact]
    var delFacts = isetEmpty[RFAFact]
    var byPassFlag = true
    val typs = p.getParamTypes
    if(typs.size == 1) byPassFlag = false
    (newFacts, delFacts, byPassFlag)
  }
}