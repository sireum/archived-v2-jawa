/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.reachability

import org.sireum.jawa.JawaMethod
import org.sireum.jawa.alir.pta.suspark.PointerAssignmentGraph
import org.sireum.jawa.alir.controlFlowGraph.InterproceduralControlFlowGraph
import org.sireum.jawa.alir.pta.suspark.PtaNode
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
	def getReachableMethods(procedures : Set[JawaMethod], timer : Option[MyTimer] = None) : Set[JawaMethod] = {
    val idfg = InterproceduralSuperSpark(procedures, timer)
    idfg.icfg.getCallGraph.getReachableMethods(procedures.map(_.getSignature)).map(Center.getMethodWithoutFailing(_))
	}
	
	def getReachableMethodsBySBCG(procedures : Set[JawaMethod], wholeProcs : Set[JawaMethod], par : Boolean) : Set[JawaMethod] = {
	  SignatureBasedCallGraph.getReachableMethods(procedures, wholeProcs, par)
	}
	
	def getBackwardReachability(apiSigs : Set[String], par : Boolean) : Map[String, Set[JawaMethod]] = {
	  BackwardCallChain.getReachableMethods(apiSigs, par)
	}
	
	def getBackwardReachability(apiSig : String, par : Boolean) : Set[JawaMethod] = {
	  BackwardCallChain.getReachableMethods(apiSig, par)
	}
	
	def getBackwardReachabilityForSubSig(apiSubSig : String, par : Boolean) : Set[JawaMethod] = {
	  BackwardCallChain.getReachableMethodsBySubSig(apiSubSig, par)
	}
}