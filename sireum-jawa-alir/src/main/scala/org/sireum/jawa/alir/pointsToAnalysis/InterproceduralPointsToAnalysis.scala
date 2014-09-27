/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.pointsToAnalysis

import org.sireum.util._
import java.io._
import org.sireum.jawa._
import org.sireum.jawa.alir._
import org.sireum.jawa.alir.controlFlowGraph.CGNode
import org.sireum.jawa.alir.controlFlowGraph.InterproceduralControlFlowGraph


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class InterproceduralPointsToAnalysis {
  
  def buildAppOnly(entryPoints : ISet[JawaProcedure]) : InterproceduralControlFlowGraph[CGNode] = {
    val pag = new PointerAssignmentGraph[PtaNode]()
    val cg = new InterproceduralControlFlowGraph[CGNode]
    pta(pag, cg, entryPoints, false)
    val result = cg
    result
  }

  def buildWholeProgram(entryPoints : ISet[JawaProcedure]) : InterproceduralControlFlowGraph[CGNode] = {
    val pag = new PointerAssignmentGraph[PtaNode]()
    val cg = new InterproceduralControlFlowGraph[CGNode]
    pta(pag, cg, entryPoints, true)
    val result = cg
    result
  }
  
  def pta(pag : PointerAssignmentGraph[PtaNode],
          cg : InterproceduralControlFlowGraph[CGNode],
          entryPoints : Set[JawaProcedure],
          wholeProgram : Boolean) = {
    entryPoints.foreach{
		  ep =>
		    if(ep.isConcrete){
		      if(!ep.hasProcedureBody)ep.resolveBody
		    	doPTA(ep, pag, cg, wholeProgram)
		    }
    }
  }
  
  def doPTA(ep : JawaProcedure,
            pag : PointerAssignmentGraph[PtaNode],
            cg : InterproceduralControlFlowGraph[CGNode],
            wholeProgram : Boolean) : Unit = {
    val points = new PointsCollector().points(ep.getSignature, ep.getProcedureBody)
    val context : Context = new Context(pag.K_CONTEXT)
    pag.constructGraph(ep, points, context.copy)
    cg.collectCfgToBaseGraph(ep, context.copy)
    workListPropagation(pag, cg, wholeProgram)
  }
  
  private def processStaticInfo(pag : PointerAssignmentGraph[PtaNode], cg : InterproceduralControlFlowGraph[CGNode], wholeProgram : Boolean) = {
    pag.processObjectAllocation
    val staticCallees = pag.processStaticCall
    staticCallees.foreach{
      callee=>
        if(wholeProgram || callee.calleeProc.getDeclaringRecord.isApplicationRecord)
        	extendGraphWithConstructGraph(callee.calleeProc, callee.pi, callee.node.getContext.copy, pag, cg)
    }
  }
  
  def workListPropagation(pag : PointerAssignmentGraph[PtaNode],
		  					 cg : InterproceduralControlFlowGraph[CGNode], wholeProgram : Boolean) : Unit = {
    processStaticInfo(pag, cg, wholeProgram)
    while (!pag.worklist.isEmpty) {
      while (!pag.worklist.isEmpty) {
      	val srcNode = pag.worklist.remove(0)
      	srcNode match{
      	  case ofbnr : PtaFieldBaseNodeR => // e.g. q = ofbnr.f; edge is ofbnr.f -> q
      	    val f = ofbnr.fieldNode
      	    pag.successorEdges(f).foreach{
      	    	edge => //edge is FIELD_LOAD type
		      	    val dstNode = pag.successor(edge)
		  	        if(pag.pointsToMap.isDiff(f, dstNode)) pag.worklist += dstNode
		  	        pag.pointsToMap.propagatePointsToSet(f, dstNode)
      	    }
      	  case _ =>
      	}
  	    pag.successorEdges(srcNode).foreach{
      	  edge =>
      	    pag.getEdgeType(edge) match{
      	      case pag.EdgeType.TRANSFER => // e.g. L0: p = q; L1:  r = p; edge is p@L0 -> p@L1
      	        val dstNode = pag.successor(edge)
      	        if(pag.pointsToMap.isDiffForTransfer(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	          val d = pag.pointsToMap.getDiff(srcNode, dstNode)
      	          pag.pointsToMap.transferPointsToSet(srcNode, dstNode)
//      	          checkAndDoModelOperation(dstNode, pag)
      	          checkAndDoCall(dstNode, d, pag, cg, wholeProgram)
      	        }
      	      case pag.EdgeType.ASSIGNMENT => // e.g. q = p; Edge: p -> q
      	        val dstNode = pag.successor(edge)
      	        if(pag.pointsToMap.isDiff(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	          pag.pointsToMap.propagatePointsToSet(srcNode, dstNode)
      	        }
      	      case pag.EdgeType.FIELD_STORE => // e.g. r.f = q; Edge: q -> r.f
      	        val dstNode = pag.successor(edge).asInstanceOf[PtaFieldNode]
      	        pag.pointsToMap.propagateFieldStorePointsToSet(srcNode, dstNode)
      	      case pag.EdgeType.ARRAY_LOAD => // e.g. q = p[i]; Edge: p[i] -> q
      	        val dstNode = pag.successor(edge)
      	        if(pag.pointsToMap.isDiff(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	        	pag.pointsToMap.propagatePointsToSet(srcNode, dstNode)
      	        }
      	      case pag.EdgeType.ARRAY_STORE => // e.g. r[i] = q; Edge: q -> r[i]
      	        val dstNode = pag.successor(edge).asInstanceOf[PtaArrayNode]
      	        if(!pag.pointsToMap.contained(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	        	pag.pointsToMap.propagateArrayStorePointsToSet(srcNode, dstNode)
      	        }
      	      case pag.EdgeType.GLOBAL_LOAD => // e.g. q = @@p; Edge: @@p -> q
      	        val dstNode = pag.successor(edge)
      	        if(pag.pointsToMap.isDiff(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	        	pag.pointsToMap.propagatePointsToSet(srcNode, dstNode)
      	        }
      	      case pag.EdgeType.GLOBAL_STORE => // e.g. @@r = q; Edge: q -> @@r
      	        val dstNode = pag.successor(edge).asInstanceOf[PtaGlobalVarNode]
      	        if(!pag.pointsToMap.contained(srcNode, dstNode)){
      	          pag.worklist += dstNode
      	        	pag.pointsToMap.propagateGlobalStorePointsToSet(srcNode, dstNode)
      	        }
      	      case _ =>
      	    }
      	}
      }
      pag.edges.foreach{
	      edge =>
	        pag.getEdgeType(edge) match{
	          case pag.EdgeType.FIELD_STORE => // q -> r.f
	            pag.pointsToMap.propagateFieldStorePointsToSet(edge.source, edge.target.asInstanceOf[PtaFieldNode])
	          case pag.EdgeType.ARRAY_STORE => // e.g. r[i] = q; Edge: q -> r[i]
    	        if(!pag.pointsToMap.pointsToSetOfArrayBaseNode(edge.target.asInstanceOf[PtaArrayNode]).isEmpty
    	            && !pag.pointsToMap.contained(edge.source, edge.target)){
    	          pag.worklist += edge.target
    	        	pag.pointsToMap.propagateArrayStorePointsToSet(edge.source, edge.target.asInstanceOf[PtaArrayNode])
    	        }
    	      case pag.EdgeType.GLOBAL_STORE => // e.g. @@r = q; Edge: q -> @@r
    	        if(!pag.pointsToMap.contained(edge.source, edge.target)){
    	          pag.worklist += edge.target
    	        	pag.pointsToMap.propagateGlobalStorePointsToSet(edge.source, edge.target.asInstanceOf[PtaGlobalVarNode])
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
      							d : MSet[PTAInstance],
      							pag : PointerAssignmentGraph[PtaNode],
      							cg : InterproceduralControlFlowGraph[CGNode],
      							wholeProgram : Boolean) = {
    val piOpt = pag.recvInverse(node)
    piOpt match {
      case Some(pi) =>
        val callerContext : Context = node.getContext
        val calleeSet : MSet[JawaProcedure] = msetEmpty
        if(pi.typ.equals("direct")){
          calleeSet += pag.getDirectCallee(pi)
        } else if(pi.typ.equals("super")){
          calleeSet ++= pag.getSuperCalleeSet(d, pi)
        } else {
          calleeSet ++= pag.getVirtualCalleeSet(d, pi)
        }
        calleeSet.foreach(
          callee => {
            if(wholeProgram || callee.getDeclaringRecord.isApplicationRecord)
            	extendGraphWithConstructGraph(callee, pi, callerContext.copy, pag, cg)
          }  
        )
        processStaticInfo(pag, cg, wholeProgram)
      case None =>
    }
  }
  
  
  def extendGraphWithConstructGraph(calleeProc : JawaProcedure, 
      															pi : PointI, 
      															callerContext : Context,
      															pag : PointerAssignmentGraph[PtaNode], 
      															cg : InterproceduralControlFlowGraph[CGNode]) = {
    val calleeSig = calleeProc.getSignature
    if(!pag.isProcessed(calleeProc, callerContext)){
    	val points = new PointsCollector().points(calleeProc.getSignature, calleeProc.getProcedureBody)
      pag.constructGraph(calleeProc, points, callerContext.copy)
      cg.collectCfgToBaseGraph(calleeProc, callerContext.copy)
    }
    val procPoint = pag.getPointProc(calleeProc, callerContext)
    require(procPoint != null)
    pag.extendGraph(procPoint, pi, callerContext.copy)
    val callersig = pi.owner
    cg.setCallMap(callersig, calleeSig)
  	cg.extendGraph(calleeSig, callerContext.copy)
  }
}