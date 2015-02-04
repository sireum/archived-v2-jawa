/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.model

import org.sireum.jawa.JawaProcedure
import org.sireum.util._
import org.sireum.jawa.alir.reachingFactsAnalysis._
import org.sireum.jawa.Center
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.ClassInstance
import org.sireum.jawa.alir.JawaAlirInfoProvider

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object UnknownCallModel {
	 def isUnknownCall(p : JawaProcedure) : Boolean = p.isUnknown
	 
	 def doUnknownCall(s : ISet[RFAFact], p : JawaProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  (newFacts, delFacts, byPassFlag)
	}
}