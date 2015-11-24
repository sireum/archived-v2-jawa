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
import org.sireum.jawa.alir.controlFlowGraph.ICFGInvokeNode
import org.sireum.jawa.alir.interProcedural.Callee
import org.sireum.jawa.alir.interProcedural.InstanceCallee


/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object InterproceduralSuperSpark {
  
  def apply
  (global: Global, 
   entryPoints: ISet[JawaMethod],
   timer: Option[MyTimer]): InterProceduralDataFlowGraph = build(global, entryPoints, timer)
  
  type N = ICFGNode
  
  def build //
  (global: Global, 
   entryPoints: ISet[JawaMethod],
   timer: Option[MyTimer]) //
   : InterProceduralDataFlowGraph = {
    val pag = new PointerAssignmentGraph[PtaNode]()
    val icfg = new InterproceduralControlFlowGraph[N]
    pta(global, pag, icfg, entryPoints, timer)
    InterProceduralDataFlowGraph(icfg, pag.pointsToMap)
  }
  
  def pta(
      global: Global,
      pag: PointerAssignmentGraph[PtaNode],
      icfg: InterproceduralControlFlowGraph[N],
      entryPoints: Set[JawaMethod],
      timer: Option[MyTimer]) = {
    entryPoints.foreach{
      ep =>
        doPTA(global, ep, pag, icfg, timer)
    }
  }
  
  def doPTA(
      global: Global,
      ep: JawaMethod,
      pag: PointerAssignmentGraph[PtaNode],
      icfg: InterproceduralControlFlowGraph[N],
      timer: Option[MyTimer]): Unit = {
    val points = new PointsCollector().points(ep.getSignature, ep.getBody)
    val context: Context = new Context
    pag.constructGraph(ep, points, context.copy)
    icfg.collectCfgToBaseGraph(ep, context.copy)
    workListPropagation(global, pag, icfg, timer)
  }
  
  private def processStaticInfo(global: Global, pag: PointerAssignmentGraph[PtaNode], icfg: InterproceduralControlFlowGraph[N]) = {
    pag.processObjectAllocation
    val staticCallees = pag.processStaticCall(global)
    staticCallees.foreach{
      case (pi, callee, context) =>
        var bypassFlag = false
        icfg.getCallGraph.addCall(pi.ownerSig, callee.callee.getSignature)
        if(!PTAScopeManager.shouldBypass(callee.callee.getDeclaringClass)) {
          extendGraphWithConstructGraph(callee, callee.pi, callee.node.getContext.copy, pag, icfg)
        } else {
          pag.handleModelCall(pi, context, callee)
          bypassFlag = true
        }
        val callNode = icfg.getICFGCallNode(callee.node.context).asInstanceOf[ICFGInvokeNode]
        callNode.addCallee(callee)
        val returnNode = icfg.getICFGReturnNode(callee.node.context).asInstanceOf[ICFGInvokeNode]
        returnNode.addCallee(callee)
        if(!bypassFlag) {
          icfg.deleteEdge(callNode, returnNode)
        }
    }
  }
  
  def workListPropagation(
      global: Global,
      pag: PointerAssignmentGraph[PtaNode],
      icfg: InterproceduralControlFlowGraph[N],
      timer: Option[MyTimer]): Unit = {
    processStaticInfo(global, pag, icfg)
    while (!pag.worklist.isEmpty) {
      while (!pag.worklist.isEmpty) {
        if(timer.isDefined) timer.get.ifTimeoutThrow
        val srcNode = pag.worklist.remove(0)
        srcNode.point match {
          case pbr: PointBaseR => // e.g. q = ofbnr.f; edge is ofbnr.f -> q
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
            pag.getEdgeType(edge) match {
              case pag.EdgeType.TRANSFER => // e.g. L0: p = q; L1:  r = p; edge is p@L0 -> p@L1
                val dstNode = pag.successor(edge)
                if(pag.pointsToMap.isDiff(srcNode, dstNode)) {
                  pag.worklist += dstNode
                  val d = pag.pointsToMap.getDiff(srcNode, dstNode)
                  pag.pointsToMap.transferPointsToSet(srcNode, dstNode)
                  checkAndDoCall(global, dstNode, d, pag, icfg)
                }
              case pag.EdgeType.THIS_TRANSFER => // e.g. L0: Call temp = foo(v1, v2); edge is v1@L0 -> foo.x@Lx
                val dstNode = pag.successor(edge)
                if(pag.pointsToMap.isDiff(srcNode, dstNode)){
                  pag.worklist += dstNode
                  val d = pag.pointsToMap.getDiff(srcNode, dstNode) //TODO we can further refine this by filter only feasible instance to pass
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
              case pag.EdgeType.STATIC_FIELD_LOAD => // e.g. q = @@p; Edge: @@p -> q
                val dstNode = pag.successor(edge)
                if(pag.pointsToMap.isDiff(srcNode, dstNode)){
                  pag.worklist += dstNode
                  pag.pointsToMap.propagatePointsToSet(srcNode, dstNode)
                }
              case pag.EdgeType.STATIC_FIELD_STORE => // e.g. @@r = q; Edge: q -> @@r
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
            case pag.EdgeType.STATIC_FIELD_STORE => // e.g. @@r = q; Edge: q -> @@r
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
            case pag.EdgeType.STATIC_FIELD_LOAD => // e.g. q = @@p; Edge: @@p -> q
              if(pag.pointsToMap.isDiff(edge.source, edge.target)){
                pag.worklist += edge.target
                pag.pointsToMap.propagatePointsToSet(edge.source, edge.target)
              }
            case _ =>
          }
      }
    }
  }
  
  def checkAndDoCall(
      global: Global,
      node: PtaNode,
      d: ISet[Instance],
      pag: PointerAssignmentGraph[PtaNode],
      icfg: InterproceduralControlFlowGraph[N]) = {
    val piOpt = pag.recvInverse(node)
    piOpt match {
      case Some(pi) =>
        val callerContext: Context = node.getContext
        val calleeSet: MSet[Callee] = msetEmpty
        if(pi.invokeTyp.equals("direct")){
          calleeSet ++= pag.getDirectCallee(global, d, pi)
        } else if(pi.invokeTyp.equals("super")){
          calleeSet ++= pag.getSuperCalleeSet(global, d, pi)
        } else {
          calleeSet ++= pag.getVirtualCalleeSet(global, d, pi)
        }
        var bypassflag = false
        calleeSet.foreach(
          callee => {
            icfg.getCallGraph.addCall(pi.ownerSig, callee.callee.getSignature)
            if(!PTAScopeManager.shouldBypass(callee.callee.getDeclaringClass)) {
              extendGraphWithConstructGraph(callee, pi, callerContext.copy, pag, icfg)
            } else {
              pag.handleModelCall(pi, callerContext, callee)
              bypassflag = true
            }
          }
        )
        if(calleeSet.isEmpty) bypassflag = true
        val callNode = icfg.getICFGCallNode(callerContext).asInstanceOf[ICFGInvokeNode]
        callNode.addCallees(calleeSet.toSet)
        val returnNode = icfg.getICFGReturnNode(callerContext).asInstanceOf[ICFGInvokeNode]
        returnNode.addCallees(calleeSet.toSet)
        if(!bypassflag){
          icfg.deleteEdge(callNode, returnNode)
        }
        processStaticInfo(global, pag, icfg)
      case None =>
    }
  }
  
  def extendGraphWithConstructGraph(callee: Callee, 
      pi: Point with Invoke, 
      callerContext: Context,
      pag: PointerAssignmentGraph[PtaNode], 
      icfg: InterproceduralControlFlowGraph[N]) = {
    val calleeProc = callee.callee
    val calleeSig = calleeProc.getSignature
    if(!pag.isProcessed(calleeProc, callerContext)){
      val points = new PointsCollector().points(calleeProc.getSignature, calleeProc.getBody)
      pag.constructGraph(calleeProc, points, callerContext.copy)
      icfg.collectCfgToBaseGraph(calleeProc, callerContext.copy)
    }
    val methodPoint = pag.getPointMethod(calleeProc, callerContext)
    require(methodPoint != null)
    pag.extendGraph(methodPoint, pi, callerContext.copy)
    val callersig = pi.ownerSig
    icfg.getCallGraph.addCall(callersig, calleeSig)
    icfg.extendGraph(calleeSig, callerContext.copy)
  }
}