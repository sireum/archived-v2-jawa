/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.pta.suspark

import org.sireum.util._
import java.io._
import org.sireum.jawa._
import org.sireum.jawa.alir._
import org.sireum.jawa.alir.controlFlowGraph.ICFGNode
import org.sireum.jawa.alir.controlFlowGraph.InterproceduralControlFlowGraph
import org.sireum.jawa.alir.util.CallHandler
import org.sireum.jawa.alir.pta.PTAScopeManager
import org.sireum.jawa.alir.pta.PTAInstance
import org.sireum.jawa.util.MyTimer
import org.sireum.jawa.alir.pta.Instance
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object InterproceduralSuperSpark {
  
  def apply(entryPoints : ISet[JawaProcedure], timer : Option[MyTimer]) : InterProceduralDataFlowGraph = build(entryPoints, timer)
  
  type N = ICFGNode
  
  def build(entryPoints : ISet[JawaProcedure], timer : Option[MyTimer]) : InterProceduralDataFlowGraph = {
    val pag = new PointerAssignmentGraph[PtaNode]()
    val icfg = new InterproceduralControlFlowGraph[N]
    pta(pag, icfg, entryPoints, timer)
    InterProceduralDataFlowGraph(icfg, pag.pointsToMap)
  }
  
  def pta(pag : PointerAssignmentGraph[PtaNode],
          icfg : InterproceduralControlFlowGraph[N],
          entryPoints : Set[JawaProcedure],
          timer : Option[MyTimer]) = {
    entryPoints.foreach{
		  ep =>
		    if(ep.isConcrete){
		      if(!ep.hasProcedureBody)ep.resolveBody
		    	doPTA(ep, pag, icfg, timer)
		    }
    }
  }
  
  def doPTA(ep : JawaProcedure,
            pag : PointerAssignmentGraph[PtaNode],
            icfg : InterproceduralControlFlowGraph[N],
            timer : Option[MyTimer]) : Unit = {
    val points = new PointsCollector().points(ep.getSignature, ep.getProcedureBody)
    val context : Context = new Context(GlobalConfig.ICFG_CONTEXT_K)
    pag.constructGraph(ep, points, context.copy)
    icfg.collectCfgToBaseGraph(ep, context.copy)
    workListPropagation(pag, icfg, timer)
  }
  
  private def processStaticInfo(pag : PointerAssignmentGraph[PtaNode], icfg : InterproceduralControlFlowGraph[N]) = {
    pag.processObjectAllocation
    val staticCallees = pag.processStaticCall
    staticCallees.foreach{
      callee=>
        if(!PTAScopeManager.shouldBypass(callee.callee.getDeclaringRecord))
        	extendGraphWithConstructGraph(callee.callee, callee.pi, callee.node.getContext.copy, pag, icfg)
    }
  }
  
  def workListPropagation(pag : PointerAssignmentGraph[PtaNode],
		  					 icfg : InterproceduralControlFlowGraph[N],
                 timer : Option[MyTimer]) : Unit = {
    processStaticInfo(pag, icfg)
    while (!pag.worklist.isEmpty) {
      while (!pag.worklist.isEmpty) {
        if(timer.isDefined) timer.get.ifTimeoutThrow
      	val srcNode = pag.worklist.remove(0)
      	srcNode.point match{
      	  case pbr : PointBaseR => // e.g. q = ofbnr.f; edge is ofbnr.f -> q
      	    val fp = pbr.getFieldPoint
            val fNode = pag.getNode(fp, srcNode.context)
      	    pag.successorEdges(fNode).foreach{
      	    	edge => //edge is FIELD_LOAD type
		      	    val dstNode = pag.successor(edge)
		  	        if(pag.pointsToMap.isDiff(fNode, dstNode)) pag.worklist += dstNode
		  	        pag.pointsToMap.propagatePointsToSet(fNode, dstNode)
      	    }
      	  case _ =>
      	}
  	    pag.successorEdges(srcNode).foreach{
      	  edge =>
      	    pag.getEdgeType(edge) match{
      	      case pag.EdgeType.TRANSFER => // e.g. L0: p = q; L1:  r = p; edge is p@L0 -> p@L1
      	        val dstNode = pag.successor(edge)
      	        if(pag.pointsToMap.isDiff(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	          val d = pag.pointsToMap.getDiff(srcNode, dstNode)
      	          pag.pointsToMap.transferPointsToSet(srcNode, dstNode)
      	          checkAndDoCall(dstNode, d, pag, icfg)
      	        }
              case pag.EdgeType.THIS_TRANSFER => // e.g. L0: Call temp = foo(v1, v2); edge is v1@L0 -> foo.x@Lx
                val dstNode = pag.successor(edge)
                if(pag.pointsToMap.isThisDiff(srcNode, dstNode)){
                  pag.worklist += dstNode
                  val d = pag.pointsToMap.getThisDiff(srcNode, dstNode)
                  pag.pointsToMap.transferPointsToSet(dstNode, d)
                }
      	      case pag.EdgeType.ASSIGNMENT => // e.g. q = p; Edge: p -> q
      	        val dstNode = pag.successor(edge)
      	        if(pag.pointsToMap.isDiff(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	          pag.pointsToMap.propagatePointsToSet(srcNode, dstNode)
      	        }
      	      case pag.EdgeType.FIELD_STORE => // e.g. r.f = q; Edge: q -> r.f
      	        val dstNode = pag.successor(edge)
      	        pag.pointsToMap.propagatePointsToSet(srcNode, dstNode)
      	      case pag.EdgeType.ARRAY_LOAD => // e.g. q = p[i]; Edge: p[i] -> q
      	        val dstNode = pag.successor(edge)
      	        if(pag.pointsToMap.isDiff(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	        	pag.pointsToMap.propagatePointsToSet(srcNode, dstNode)
      	        }
      	      case pag.EdgeType.ARRAY_STORE => // e.g. r[i] = q; Edge: q -> r[i]
      	        val dstNode = pag.successor(edge)
      	        if(!pag.pointsToMap.contained(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	        	pag.pointsToMap.propagatePointsToSet(srcNode, dstNode)
      	        }
      	      case pag.EdgeType.GLOBAL_LOAD => // e.g. q = @@p; Edge: @@p -> q
      	        val dstNode = pag.successor(edge)
      	        if(pag.pointsToMap.isDiff(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	        	pag.pointsToMap.propagatePointsToSet(srcNode, dstNode)
      	        }
      	      case pag.EdgeType.GLOBAL_STORE => // e.g. @@r = q; Edge: q -> @@r
      	        val dstNode = pag.successor(edge)
      	        if(!pag.pointsToMap.contained(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	        	pag.pointsToMap.propagatePointsToSet(srcNode, dstNode)
      	        }
      	      case _ =>
      	    }
      	}
      }
      pag.edges.foreach{
	      edge =>
	        pag.getEdgeType(edge) match{
	          case pag.EdgeType.FIELD_STORE => // q -> r.f
	            pag.pointsToMap.propagatePointsToSet(edge.source, edge.target)
	          case pag.EdgeType.ARRAY_STORE => // e.g. r[i] = q; Edge: q -> r[i]
    	        if(!pag.pointsToMap.pointsToSet(edge.target).isEmpty
    	            && !pag.pointsToMap.contained(edge.source, edge.target)){
    	          pag.worklist += edge.target
    	        	pag.pointsToMap.propagatePointsToSet(edge.source, edge.target)
    	        }
    	      case pag.EdgeType.GLOBAL_STORE => // e.g. @@r = q; Edge: q -> @@r
    	        if(!pag.pointsToMap.contained(edge.source, edge.target)){
    	          pag.worklist += edge.target
    	        	pag.pointsToMap.propagatePointsToSet(edge.source, edge.target)
    	        }
	          case _ =>
	        }
	    }
      pag.edges.foreach{
	      edge =>
	        pag.getEdgeType(edge) match{
	          case pag.EdgeType.FIELD_LOAD => // p.f -> q
	  	        if(pag.pointsToMap.isDiff(edge.source, edge.target)){
	  	          pag.worklist += edge.target
	  	          pag.pointsToMap.propagatePointsToSet(edge.source, edge.target)
	  	        }
	  	      case pag.EdgeType.ARRAY_LOAD => // e.g. q = p[i]; Edge: p[i] -> q
    	        if(pag.pointsToMap.isDiff(edge.source, edge.target)){
    	          pag.worklist += edge.target
    	        	pag.pointsToMap.propagatePointsToSet(edge.source, edge.target)
    	        }
    	      case pag.EdgeType.GLOBAL_LOAD => // e.g. q = @@p; Edge: @@p -> q
    	        if(pag.pointsToMap.isDiff(edge.source, edge.target)){
    	          pag.worklist += edge.target
    	        	pag.pointsToMap.propagatePointsToSet(edge.source, edge.target)
    	        }
	          case _ =>
	        }
	    }
    }
  }
  
  def checkAndDoCall(node : PtaNode,
      							d : ISet[Instance],
      							pag : PointerAssignmentGraph[PtaNode],
      							icfg : InterproceduralControlFlowGraph[N]) = {
    val piOpt = pag.recvInverse(node)
    piOpt match {
      case Some(pi) =>
        val callerContext : Context = node.getContext
        val calleeSet : MSet[JawaProcedure] = msetEmpty
        if(pi.invokeTyp.equals("direct")){
          calleeSet += pag.getDirectCallee(pi)
        } else if(pi.invokeTyp.equals("super")){
          calleeSet ++= pag.getSuperCalleeSet(d, pi)
        } else {
          calleeSet ++= pag.getVirtualCalleeSet(d, pi)
        }
        var bypassflag = true
        calleeSet.foreach(
          callee => {
            if(!PTAScopeManager.shouldBypass(callee.getDeclaringRecord))
            	extendGraphWithConstructGraph(callee, pi, callerContext.copy, pag, icfg)
            else bypassflag = false
          }  
        )
        if(!bypassflag){
          val callNode = icfg.getICFGCallNode(callerContext)
          val returnNode = icfg.getICFGReturnNode(callerContext)
          icfg.addEdge(callNode, returnNode)
        }
        processStaticInfo(pag, icfg)
      case None =>
    }
  }
  
  
  def extendGraphWithConstructGraph(calleeProc : JawaProcedure, 
      															pi : Point with Invoke, 
      															callerContext : Context,
      															pag : PointerAssignmentGraph[PtaNode], 
      															icfg : InterproceduralControlFlowGraph[N]) = {
    val calleeSig = calleeProc.getSignature
    if(!pag.isProcessed(calleeProc, callerContext)){
    	val points = new PointsCollector().points(calleeProc.getSignature, calleeProc.getProcedureBody)
      pag.constructGraph(calleeProc, points, callerContext.copy)
      icfg.collectCfgToBaseGraph(calleeProc, callerContext.copy)
    }
    val procPoint = pag.getPointProc(calleeProc, callerContext)
    require(procPoint != null)
    pag.extendGraph(procPoint, pi, callerContext.copy)
    val callersig = pi.ownerSig
    icfg.getCallGraph.addCall(callersig, calleeSig)
  	icfg.extendGraph(calleeSig, callerContext.copy)
  }
}