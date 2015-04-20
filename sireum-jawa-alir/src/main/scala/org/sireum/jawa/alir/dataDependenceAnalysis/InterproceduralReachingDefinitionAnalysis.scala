/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.dataDependenceAnalysis

import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.util._
import org.sireum.jawa.alir.Context
import org.sireum.pilar.ast.CallJump
import org.sireum.pilar.ast.ProcedureDecl
import org.sireum.jawa.GlobalConfig
import org.sireum.alir.VarSlot
import org.sireum.alir.InitDefDesc
import org.sireum.jawa.PilarAstHelper
import org.sireum.alir._
import org.sireum.jawa.alir.reachingDefinitionAnalysis.JawaReachingDefinitionAnalysis
import org.sireum.jawa.alir.JawaAlirInfoProvider
import org.sireum.jawa.Center
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralMonotoneDataFlowAnalysisFramework
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralMonotonicFunction
import org.sireum.jawa.alir.dataFlowAnalysis.CallResolver

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object InterproceduralReachingDefinitionAnalysis {
  type RDFact = JawaReachingDefinitionAnalysis.RDFact
  type IRDFact = (RDFact, Context)
  type Node = ICFGNode
  
  def apply(cg : InterproceduralControlFlowGraph[Node],
      parallel : Boolean = false,
      timer : Option[MyTimer] = None,
	    switchAsOrderedMatch : Boolean = false) = build(cg, parallel, timer, switchAsOrderedMatch)
	
	def build(
	    cg : InterproceduralControlFlowGraph[Node],
	    parallel : Boolean = false,
      timer : Option[MyTimer] = None,
	    switchAsOrderedMatch : Boolean = false) = {
    new InterproceduralReachingDefinitionAnalysis().build(cg, parallel, timer, switchAsOrderedMatch)
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class InterproceduralReachingDefinitionAnalysis {
  type RDFact = InterproceduralReachingDefinitionAnalysis.RDFact
  type IRDFact = InterproceduralReachingDefinitionAnalysis.IRDFact
	type Node = InterproceduralReachingDefinitionAnalysis.Node
	
  var cg : InterproceduralControlFlowGraph[Node] = null
  var factSet = idmapEmpty[Node, ISet[IRDFact]]
  
	def build(
	    cg : InterproceduralControlFlowGraph[Node],
	    parallel : Boolean,
      timer : Option[MyTimer],
	    switchAsOrderedMatch : Boolean) = {
	  val gen = new Gen
    val kill = new Kill
    val callr = new Callr
    this.cg = cg
    cg.nodes.foreach{
	    node =>
	      val owner = Center.getMethodWithoutFailing(node.getOwner)
	      if(!owner.isUnknown){
		      val cfg = JawaAlirInfoProvider.getCfg(owner)
		      val rda = JawaAlirInfoProvider.getRda(owner, cfg)
		      node match{
		        case cvn : ICFGVirtualNode =>
		          val rdafact = rda.entrySet(cfg.getVirtualNode(cvn.getVirtualLabel))
		          factSet.update(cvn, rdafact.map{fact => (fact, getContext(fact, cvn.getContext))})
		        case cln : ICFGLocNode =>
		          val owner = Center.getMethodWithoutFailing(cln.getOwner)
		          val rdafact = rda.entrySet(cfg.getNode(owner.getMethodBody.location(cln.getLocIndex)))
		          factSet.update(cln, rdafact.map{fact => (fact, getContext(fact, cln.getContext))})
		      }
	      }
	  }
	  val initialContext : Context = new Context(GlobalConfig.ICFG_CONTEXT_K)
    val iota : ISet[IRDFact] = isetEmpty + (((VarSlot("@@IRDA"), InitDefDesc), initialContext))
    val initial : ISet[IRDFact] = isetEmpty
    val result = InterProceduralMonotoneDataFlowAnalysisFramework[IRDFact](cg,
      true, true, false, parallel, gen, kill, callr, iota, initial, timer, switchAsOrderedMatch, None)

//    print("IRD\n")
//    print(result)
    factSet
	}
  
  private def getContext(fact : RDFact, srcContext : Context) : Context = {
    val procSig = srcContext.getMethodSig
    val tarContext = srcContext.copy.removeTopContext
    fact._2 match {
      case pdd : ParamDefDesc =>
        pdd.locUri match{
          case Some(locU) =>
            tarContext.setContext(procSig, locU)
          case None =>
            throw new RuntimeException("Unexpected ParamDefDesc: " + pdd)
        }
      case ldd : LocDefDesc => 
        ldd.locUri match {
          case Some(locU) =>
            tarContext.setContext(procSig, locU)
          case None =>
            throw new RuntimeException("Unexpected LocDefDesc: " + ldd)
        }
      case dd : DefDesc =>
        if(dd.isDefinedInitially){
          tarContext.setContext(procSig, "Entry")
        } else if(dd.isUndefined) {
          tarContext.setContext(procSig, "Entry")
        } else throw new RuntimeException("Unexpected DefDesc: " + dd)
    }
  }
  
  private def isGlobal(slot : Slot) : Boolean = 
    slot match{
	    case vs : VarSlot => vs.varUri.contains("@@")
	    case _ => false
	  }
  
  private def isDef(defDesc : DefDesc) : Boolean =
    defDesc match{
	    case ldd : LocDefDesc => true
	    case _ => false
	  }
	
	/**
	 * @author Fengguo Wei & Sankardas Roy
	 */
	class Gen extends InterProceduralMonotonicFunction[IRDFact] {
		import org.sireum.pilar.ast._
	
	  def apply(s : ISet[IRDFact], a : Assignment, currentNode : ICFGLocNode) : ISet[IRDFact] = {
		  val node = currentNode
		  val succs = cg.successors(node)
		  val globFacts = 
		    if(succs.isEmpty) isetEmpty[IRDFact]
		    else succs.map(node => factSet(node).filter(fact => isGlobal(fact._1._1) && isDef(fact._1._2))).reduce(iunion[IRDFact])
		  val globDefFacts = globFacts.filter(fact => isDef(fact._1._2))
		  val flowingGlobFacts = s.filter(fact => isGlobal(fact._1._1) && isDef(fact._1._2))
		  factSet += (node -> (factSet.getOrElse(node, isetEmpty) -- globFacts ++ flowingGlobFacts ++ globDefFacts))
		  globDefFacts
		}
	  def apply(s : ISet[IRDFact], e : Exp, currentNode : ICFGLocNode) : ISet[IRDFact] = isetEmpty
		def apply(s : ISet[IRDFact], a : Action, currentNode : ICFGLocNode) : ISet[IRDFact] = isetEmpty
	}
	
	/**
	 * @author Fengguo Wei & Sankardas Roy
	 */
	class Kill extends InterProceduralMonotonicFunction[IRDFact] {
		import org.sireum.pilar.ast._
	  def apply(s : ISet[IRDFact], a : Assignment, currentNode : ICFGLocNode) : ISet[IRDFact] = {
		  val node = currentNode
		  val succs = cg.successors(node)
		  val globDefFacts = 
		    if(succs.isEmpty) isetEmpty[IRDFact]
		    else succs.map(node => factSet(node).filter(fact => isGlobal(fact._1._1) && isDef(fact._1._2))).reduce(iunion[IRDFact])
		  val redefGlobSlots = globDefFacts.filter(fact => s.map(_._1._1).contains(fact._1._1)).map(_._1._1)
		  s.filter(f => !redefGlobSlots.contains(f._1._1))
		}
	  def apply(s : ISet[IRDFact], e : Exp, currentNode : ICFGLocNode) : ISet[IRDFact] = s
		def apply(s : ISet[IRDFact], a : Action, currentNode : ICFGLocNode) : ISet[IRDFact] = s
	}
	
	/**
	 * @author Fengguo Wei & Sankardas Roy
	 */
	class Callr extends CallResolver[IRDFact] {
	  /**
		 * It returns the facts for each callee entry node and caller return node
		 */
	  def resolveCall(s : ISet[IRDFact], cj : CallJump, callerContext : Context, cg : InterproceduralControlFlowGraph[ICFGNode]) : (IMap[ICFGNode, ISet[IRDFact]], ISet[IRDFact]) = {
      var calleeFactsMap : IMap[ICFGNode, ISet[IRDFact]] = imapEmpty
      var returnFacts : ISet[IRDFact] = isetEmpty
      val callNode = cg.getICFGCallNode(callerContext)
	    cg.successors(callNode).foreach{
	      suc =>
	        if(suc.isInstanceOf[ICFGEntryNode]){
	          calleeFactsMap += (suc -> s)
	        } else if(suc.isInstanceOf[ICFGReturnNode]){
	          returnFacts ++= s
	        }
	    }
	    (calleeFactsMap, returnFacts)
	  }
	  
	  def getAndMapFactsForCaller(calleeS : ISet[IRDFact], callerNode : ICFGNode, calleeExitNode : ICFGVirtualNode) : ISet[IRDFact] = {
	    calleeS
	  }
	  
	}
	
}