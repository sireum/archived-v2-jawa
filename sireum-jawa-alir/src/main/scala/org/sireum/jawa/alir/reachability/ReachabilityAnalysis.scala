/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.reachability

import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.alir.pta.suspark.PointerAssignmentGraph
import org.sireum.jawa.alir.controlFlowGraph.InterproceduralControlFlowGraph
import org.sireum.jawa.alir.pta.suspark.PtaNode
import org.sireum.jawa.alir.controlFlowGraph.CGNode
import org.sireum.jawa.Center
import org.sireum.jawa.alir.pta.suspark.InterproceduralSuperSpark
import org.sireum.jawa.util.MyTimer

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object ReachabilityAnalysis {
  
  /**
	 * Get all reachable procedures of given procedure set.
	 * @param procedureUris Initial procedures set
	 * @param wholeProgram Building call graph in whole program mode or not
	 * @return Set of reachable procedure resource uris from initial set
	 */
	def getReachableProcedures(procedures : Set[JawaProcedure], timer : Option[MyTimer] = None) : Set[JawaProcedure] = {
    val idfg = InterproceduralSuperSpark(procedures, timer)
    idfg.icfg.getReachableProcedures(procedures.map(_.getSignature)).map(Center.getProcedureWithoutFailing(_))
	}
	
	def getReachableProceduresBySBCG(procedures : Set[JawaProcedure], wholeProcs : Set[JawaProcedure], par : Boolean) : Set[JawaProcedure] = {
	  SignatureBasedCallGraph.getReachableProcedures(procedures, wholeProcs, par)
	}
	
	def getBackwardReachability(apiSigs : Set[String], par : Boolean) : Map[String, Set[JawaProcedure]] = {
	  BackwardCallChain.getReachableProcedures(apiSigs, par)
	}
	
	def getBackwardReachability(apiSig : String, par : Boolean) : Set[JawaProcedure] = {
	  BackwardCallChain.getReachableProcedures(apiSig, par)
	}
	
	def getBackwardReachabilityForSubSig(apiSubSig : String, par : Boolean) : Set[JawaProcedure] = {
	  BackwardCallChain.getReachableProceduresBySubSig(apiSubSig, par)
	}
}