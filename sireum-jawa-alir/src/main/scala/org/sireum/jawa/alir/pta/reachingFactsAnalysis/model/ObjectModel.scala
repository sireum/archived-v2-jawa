/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.pta.reachingFactsAnalysis.model

import org.sireum.jawa._
import org.sireum.util._
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.JawaAlirInfoProvider
import org.sireum.jawa.alir.pta._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object ObjectModel {
  val TITLE = "ObjectModel"
	def isObject(r: JawaClass): Boolean = r.getName == "java.lang.Object"
	  
	def doObjectCall(s: PTAResult, p: JawaMethod, args: List[String], retVars: Seq[String], currentContext: Context): (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  p.getSignature.signature match{
      case "Ljava/lang/Object;.<init>:()V" =>
        byPassFlag = false
	    case "Ljava/lang/Object;.getClass:()Ljava/lang/Class;" =>
        require(retVars.size == 1)
        objectGetClass(s, args, retVars(0), currentContext) match{case (n, d) => newFacts ++= n; delFacts ++= d}
        byPassFlag = false
      case _ =>
	  }
	  (newFacts, delFacts, byPassFlag)
	}
	
	private def objectGetClass(s: PTAResult, args: List[String], retVar: String, currentContext: Context): (ISet[RFAFact], ISet[RFAFact]) = {
    require(args.size > 0)
    val thisSlot = VarSlot(args(0), false, true)
    val thisValue = s.pointsToSet(thisSlot, currentContext)
    var newfacts = isetEmpty[RFAFact]
    var delfacts = isetEmpty[RFAFact]
    thisValue.foreach{
      cIns =>
        val typ = cIns.typ
        val strIns = ClassInstance(typ, cIns.defSite)
        newfacts += (RFAFact(VarSlot(retVar, false, false), strIns))
    }
    (newfacts, delfacts)
  }
	
	
}