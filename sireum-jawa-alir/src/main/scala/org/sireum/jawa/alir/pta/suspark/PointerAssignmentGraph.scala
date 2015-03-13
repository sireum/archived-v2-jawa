/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.pta.suspark

import org.sireum.util._
import org.sireum.alir.AlirGraph
import org.sireum.alir.AlirEdgeAccesses
import org.sireum.alir.AlirSuccPredAccesses
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ReachingDefinitionAnalysis
import org.jgrapht.graph.DirectedMultigraph
import org.jgrapht.EdgeFactory
import org.sireum.alir.AlirEdge
import org.jgrapht.ext.VertexNameProvider
import java.io.Writer
import org.jgrapht.ext.DOTExporter
import org.sireum.jawa.alir.Context
import org.sireum.jawa._
import org.sireum.jawa.alir.interProcedural.InterProceduralGraph
import org.sireum.jawa.alir.interProcedural.InterProceduralNode
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.alir.JawaAlirInfoProvider
import scala.collection.mutable.HashMap
import scala.collection.mutable.SynchronizedMap
import org.sireum.jawa.alir.util.CallHandler
import org.sireum.jawa.alir.interProcedural.Callee
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.VarSlot
import org.sireum.jawa.alir.pta.ArraySlot
import org.sireum.jawa.alir.pta.InstanceSlot
import org.sireum.alir.Slot
import org.sireum.jawa.alir.pta.PTAInstance
import org.sireum.jawa.alir.pta.Instance
import org.sireum.jawa.alir.pta.PTAConcreteStringInstance
import org.sireum.jawa.alir.pta.ArraySlot
import org.sireum.jawa.alir.pta.FieldSlot
import org.sireum.jawa.alir.pta.InvokeSlot

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class PointsToMap extends PTAResult {
  /**
   * e.g. L0: p = q; L1:  r = p; transfer means p@L0 -> p@L1
   */
  def transferPointsToSet(n1 : PtaNode, n2 : PtaNode) = {
    n2.getSlots(this) foreach{
      addInstances(_, n2.getContext.copy, pointsToSet(n1))
    }
  }
  /**
   * n1 -> n2 or n1.f -> n2 or n1[] -> n2, n1 -> n2.f, n1 -> n2[]
   */
  def propagatePointsToSet(n1 : PtaNode, n2 : PtaNode) = {
    n2.getSlots(this) foreach {
      slot =>
        slot match {
          case arr : ArraySlot =>
            addInstances(arr, n2.getContext, pointsToSet(n1))
          case fie : FieldSlot =>
            addInstances(fie, n2.getContext, pointsToSet(n1))
          case _ =>
            setInstances(slot, n2.getContext, pointsToSet(n1))
        }
    }
  }
//  /**
//   * 
//   */
//  def propagateFieldStorePointsToSet(n1 : PtaNode, n2 : PtaFieldNode) = {
//    pointsToSet(n2.baseNode) foreach{
//      ins =>
//        val fieldName = StringFormConverter.getFieldNameFromFieldSignature(n2.fieldName)
//        addInstances(ins.toString + fieldName, n2.baseNode.getContext, pointsToSet(n1))
//    }
//  }
//  
//  /**
//   * n1 -> n2[]
//   */
//  def propagateArrayStorePointsToSet(n1 : PtaNode, n2 : PtaNode) = {
//    pointsToSet(n2) foreach{
//      ins =>
//        addInstances(ArraySlot(ins), n2.getContext, pointsToSet(n1))
//    }
//  }
//  
//  /**
//   * n1 -> @@n2
//   */
//  def propagateGlobalStorePointsToSet(n1 : PtaNode, n2 : PtaNode) = {
//    addInstances(VarSlot(n2.name), n2.getContext, pointsToSet(n1))
//  }
  
	/**
	 * n or n.f or n[] or @@n
	 */
  def pointsToSet(n : PtaNode) : ISet[Instance] = {
    val slots = n.getSlots(this)
    if(!slots.isEmpty){
      slots.map {
        s =>
          pointsToSet(s, n.getContext)
      }.reduce(iunion[Instance])
    } else isetEmpty
  }

  def isDiff(n1 : PtaNode, n2 : PtaNode) : Boolean = {
    pointsToSet(n1) != pointsToSet(n2)
  }
  
//  def isDiffForTransfer(n1 : PtaNode, n2 : PtaNode) : Boolean = {
//    n1 match{
//      case pan1 : PtaArrayNode =>
//        n2 match{
//          case pan2 : PtaArrayNode =>
//            pointsToSet(pan1) != pointsToSet(pan2)
//          case _ =>
//            pointsToSet(pan1) != pointsToSet(n2)
//        }
//      case _=>
//        n2 match{
//          case pan2 : PtaArrayNode =>
//            pointsToSet(n1) != pointsToSet(pan2)
//          case _ =>
//            pointsToSet(n1) != pointsToSet(n2)
//        }
//    }
//  }
  
  def contained(n1 : PtaNode, n2 : PtaNode) : Boolean = {
    (pointsToSet(n1) -- pointsToSet(n2)).isEmpty
  }
  
  def getDiff(n1 : PtaNode, n2 : PtaNode) : ISet[Instance] = {
    pointsToSet(n1) diff pointsToSet(n2)
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class PointerAssignmentGraph[Node <: PtaNode]
  extends InterProceduralGraph[Node]
  with PAGConstraint{
  self=>
    
  val pointsToMap = new PointsToMap
  
  private val processed : MMap[(JawaProcedure, Context), PointProc] = new HashMap[(JawaProcedure, Context), PointProc] with SynchronizedMap[(JawaProcedure, Context), PointProc]
  
  def isProcessed(proc : JawaProcedure, callerContext : Context) : Boolean = processed.contains(proc, callerContext)
  
  def getPointProc(proc : JawaProcedure, callerContext : Context) : PointProc =  processed(proc, callerContext)
  
  def addProcessed(jp : JawaProcedure, c : Context, ps : Set[Point]) = {
    ps.foreach{
      p =>
        if(p.isInstanceOf[PointProc])
          this.processed += ((jp, c) -> p.asInstanceOf[PointProc])
    }
  }
  
  def getProcessed = this.processed
  
  private var newNodes : Set[Node] = isetEmpty
  private var newEdges : Set[Edge] = isetEmpty
  
  final case class PTACallee(callee : JawaProcedure, pi : Point with Invoke, node : Node) extends Callee
  
  def processStaticCall : ISet[PTACallee] = {
    val staticCallees = msetEmpty[PTACallee]
    newNodes.foreach{
      node =>
        if(node.point.isInstanceOf[Point with Invoke]){
          val pi = node.point.asInstanceOf[Point with Invoke]
          if(pi.invokeTyp.equals("static")){
	          val callee = getStaticCallee(pi)
	          staticCallees += PTACallee(callee, pi, node)
	        }
        }
    }
    newNodes = isetEmpty
    staticCallees.toSet
  }
  
  def processObjectAllocation = {
    newEdges.foreach{
      edge =>
        getEdgeType(edge) match{
          case EdgeType.ALLOCATION =>
            if(pointsToMap.isDiff(edge.source, edge.target)){
	            pointsToMap.propagatePointsToSet(edge.source, edge.target)
	            worklist += edge.target
            }
          case _ =>
        }
    }
    newEdges = isetEmpty
  }
  
  def addEdge(source : Node, target : Node, typ : EdgeType.Value) : Edge = {
    val edge = graph.addEdge(getNode(source), getNode(target))
    edge.setProperty(EDGE_TYPE, typ)
    edge
  }
  
  def getEdgeType(edge : Edge) : EdgeType.Value = {
    assume(edge.propertyMap.contains(EDGE_TYPE))
    edge.getProperty[EdgeType.Value](EDGE_TYPE)
  }
  
  final val EDGE_TYPE = "EdgeType"
  final val PARAM_NUM = "ParamNumber"

  /**
   * represents max number of strings in the strings set of a StringInstance
   */
//  final val K_STRING : Int = 5
  
  
  final val worklist : MList[Node] = mlistEmpty
    
  /**
   * combine two pags into one.
   */ 
  def combinePags(pag2 : PointerAssignmentGraph[Node]) = {
    pl ++= pag2.pool
    pag2.nodes.foreach(
      node=>{
        addNode(node)
      }
    )
    pag2.edges.foreach(
      edge=>{
        addEdge(edge)
      }  
    )
    this.processed ++= pag2.getProcessed
    worklist ++= pag2.worklist
  }
  
  
  /**
   * create the nodes and edges to reflect the constraints corresponding 
   * to the given program point. If a value is added to a node, then that 
   * node is added to the worklist.
   */
  def constructGraph(ap : JawaProcedure, ps : Set[Point], callerContext : Context) = {
    addProcessed(ap, callerContext.copy, ps)
    ps.foreach{
      p =>
        newNodes ++= collectNodes(ap.getSignature, p, callerContext.copy)
    }
    ps.foreach{
      p =>
        val cfg = JawaAlirInfoProvider.getCfg(ap)
        val rda = JawaAlirInfoProvider.getRda(ap, cfg)
        val constraintMap = applyConstraint(p, ps, cfg, rda)
        newEdges ++= buildingEdges(constraintMap, ap.getSignature, callerContext.copy)
    }
  }
  
  /**
   * combine proc point and relevant node
   */ 
//  def collectTrackerNodes(sig : String, pi : Point with Invoke, callerContext : Context) = {
//    val recvCallNodeOpt =
//      pi match{
//       	case vp : Point with Invoke with Dynamic =>
//	        Some(getNode(vp.recvPCall, callerContext))
//	      case _ =>
//	        None
//    	}
//    val recvReturnNodeOpt = 
//      pi match{
//        case vp : Point with Invoke with Dynamic =>
//	        Some(getNode(vp.recvPReturn, callerContext))
//	      case _ =>
//	        None
//    	}
//    val invokeNodeOpt = if(nodeExists(pi, callerContext)) Some(getNode(pi, callerContext)) else None
//    val argCallNodes = pi.argPsCall.map{case (i, p) => (i, getNode(p, callerContext))}.toMap
//    val argReturnNodes = pi.argPsReturn.map{case (i, p) => (i, getNode(p, callerContext))}.toMap
//    val ipN = InvokePointNode[Node](recvCallNodeOpt, recvReturnNodeOpt, argCallNodes, argReturnNodes, invokeNodeOpt, pi)
//    ipN.setCalleeSig(sig)
//    ipN.setContext(callerContext)
//    ipN
//  }
  

  def collectNodes(pSig : String, p : Point, callerContext : Context) : Set[Node] = {
    var nodes : Set[Node] = Set()
    val context = callerContext.copy
    p match {
      case lp : Point with Loc => context.setContext(pSig, lp.loc)
      case _ => context.setContext(pSig, p.ownerSig)
    }
    
    p match {
      case asmtP : PointAsmt =>
        val lhs = asmtP.lhs
        val rhs = asmtP.rhs
        val lhsNode = getNodeOrElse(lhs, context.copy)
        nodes += lhsNode
        val rhsNode = getNodeOrElse(rhs, context.copy)
        nodes += rhsNode
        lhs match {
          case pfl : PointFieldL =>
            val fieldNode = getNodeOrElse(pfl, context.copy)
            nodes += fieldNode
            val baseNode = getNodeOrElse(pfl.baseP, context.copy)
            nodes += baseNode
//            baseNode.asInstanceOf[PtaFieldBaseNode].fieldNode = fieldNode.asInstanceOf[PtaFieldNode]
//            fieldNode.asInstanceOf[PtaFieldNode].baseNode = baseNode.asInstanceOf[PtaFieldBaseNode]
          case _ =>
        }
        rhs match {
          case pgr : PointGlobalR =>
            val globalVarNode = getNodeOrElse(pgr, context.copy)
            nodes += globalVarNode
          case pfr : PointFieldR =>
            val fieldNode = getNodeOrElse(pfr, context.copy)
            nodes += fieldNode
            val baseNode = getNodeOrElse(pfr.baseP, context.copy)
            nodes += baseNode
//            baseNode.asInstanceOf[PtaFieldBaseNode].fieldNode = fieldNode.asInstanceOf[PtaFieldNode]
//            fieldNode.asInstanceOf[PtaFieldNode].baseNode = baseNode.asInstanceOf[PtaFieldBaseNode]
          case pso : PointStringO =>
            val ins = PTAConcreteStringInstance(pso.text, context.copy)
            pointsToMap.addInstance(InstanceSlot(ins), context.copy, ins)
          case pao : PointArrayO =>
            val ins = PTAInstance(new NormalType(pao.obj, pao.dimensions), context.copy)
            pointsToMap.addInstance(InstanceSlot(ins), context.copy, ins)
          case po : PointO =>
            val ins = PTAInstance(new NormalType(po.obj), context.copy)
            pointsToMap.addInstance(InstanceSlot(ins), context.copy, ins)
          case pi : PointI =>
            if(pi.invokeTyp.equals("static")){
              worklist += rhsNode
            }
          case _ =>
        }
      case pi : Point with Invoke =>
        pi match {
          case vp : Point with Invoke with Dynamic =>
            nodes += getNodeOrElse(vp.recvPCall, context.copy)
            nodes += getNodeOrElse(vp.recvPReturn, context.copy)
          case _ =>
            val node = getNodeOrElse(pi, context.copy)
            nodes += node
            worklist += node
        }
        val args_Entry = pi.argPsCall
        val args_Exit = pi.argPsReturn
        args_Entry.foreach{
          case (_, pa) =>
            val argNode = getNodeOrElse(pa, context.copy)
            nodes += argNode
            argNode.setProperty(PARAM_NUM, pa.index)
        }
        args_Exit.foreach{
          case (_, pa) =>
            val argNode = getNodeOrElse(pa, context.copy)
            nodes += argNode
            argNode.setProperty(PARAM_NUM, pa.index)
        }
      case procP : Point with Proc =>
        procP match {
          case vp : Point with Proc with Virtual => 
            nodes += getNodeOrElse(vp.thisPEntry, context.copy)
            nodes += getNodeOrElse(vp.thisPExit, context.copy)
          case _ =>
        }
//        procP.retVar match {
//          case Some(rev) =>
//            nodes += getNodeOrElse(rev, context.copy)
//          case None =>
//        }
        val params_Entry = procP.paramPsEntry
        val params_Exit = procP.paramPsExit
        params_Entry.foreach{
          case (_, pa) => 
            val paramNode = getNodeOrElse(pa, context.copy)
            nodes += paramNode
            paramNode.setProperty(PARAM_NUM, pa.index)
        }
        params_Exit.foreach{
          case (_, pa) =>
            val paramNode = getNodeOrElse(pa, context.copy)
            nodes += paramNode
            paramNode.setProperty(PARAM_NUM, pa.index)
        }
      case retP : PointRet =>
        nodes += getNodeOrElse(retP, context.copy)
      case _ =>
    }
    nodes
  }
  
  def buildingEdges(map : MMap[EdgeType.Value, MMap[Point, MSet[Point]]], pSig : String, context : Context) : Set[Edge] = {
    var edges : Set[Edge] = isetEmpty
    map.foreach{
      case(typ, edgeMap) =>
        edgeMap.foreach{
          case(src, dsts) =>
            val s = context.copy
            src match {
              case lp : Point with Loc => s.setContext(pSig, lp.loc)
              case _ => s.setContext(pSig, src.ownerSig)
            }
		        val srcNode = getNode(src, s)
		        dsts.foreach{
		          dst => 
		            val t = context.copy
		            dst match {
                  case lp : Point with Loc => t.setContext(pSig, lp.loc)
                  case _ => t.setContext(pSig, dst.ownerSig)
                }
		            val targetNode = getNode(dst, t)
		            if(!graph.containsEdge(srcNode, targetNode))
		              edges += addEdge(srcNode, targetNode, typ)
		        }
        }
  	}
    edges
  }
  
  def breakPiEdges(pi : Point with Invoke, calleeAccessTyp : String, srcContext : Context) = {
    pi match {
      case vp : Point with Invoke with Dynamic =>
        if(calleeAccessTyp != null && !calleeAccessTyp.contains("NATIVE")){
          val srcNode = getNode(vp.recvPCall, srcContext.copy)
          val targetNode = getNode(vp.recvPReturn, srcContext.copy)
          if(hasEdge(srcNode, targetNode))
            deleteEdge(srcNode, targetNode)
        }
      case _ =>
    }
    
    pi.argPsCall foreach{
      case (_, aCall) =>
        pi.argPsReturn foreach{
          case (_, aReturn) =>
            if(aCall.index == aReturn.index){
              val srcNode = getNode(aCall, srcContext.copy)
              val targetNode = getNode(aReturn, srcContext.copy)
              if(hasEdge(srcNode, targetNode))
                deleteEdge(srcNode, targetNode)
            }
        }
        
    }
  }
  
  private def connectCallEdges(met : Point with Proc, pi : Point with Invoke, srcContext : Context) ={
    val targetContext = srcContext.copy
    targetContext.setContext(met.procSig, met.ownerSig)
    met.paramPsEntry.foreach{
      case (_, paramp) => 
        pi.argPsCall.foreach{
          case (_, argp) =>
            if(paramp.index == argp.index){
              val srcNode = getNode(argp, srcContext.copy)
              val targetNode = getNode(paramp, targetContext.copy)
              worklist += srcNode
              if(!graph.containsEdge(srcNode, targetNode))
                addEdge(srcNode, targetNode, EdgeType.TRANSFER)
            }
          
        }
    }
    met.paramPsExit.foreach{
      case (_, paramp) =>
        pi.argPsReturn.foreach{
          case (_, argp) =>
            if(paramp.index == argp.index){
              val srcNode = getNode(argp, srcContext.copy)
              val targetNode = getNode(paramp, targetContext.copy)
              worklist += srcNode
              if(!graph.containsEdge(srcNode, targetNode))
                addEdge(srcNode, targetNode, EdgeType.TRANSFER)
            }
          
        }
    }
    
    met match {
      case vp : Point with Proc with Virtual =>
        assume(pi.isInstanceOf[PointI])
        val srcNodeCall = getNode(pi.asInstanceOf[PointI].recvPCall, srcContext.copy)
        val targetNodeEntry = getNode(vp.thisPEntry, targetContext.copy)
        worklist += srcNodeCall
        if(!graph.containsEdge(srcNodeCall, targetNodeEntry))
          addEdge(srcNodeCall, targetNodeEntry, EdgeType.TRANSFER)
        val srcNodeExit = getNode(vp.thisPExit, targetContext.copy)
        val targetNodeReturn = getNode(pi.asInstanceOf[PointI].recvPReturn, srcContext.copy)
        worklist += srcNodeExit
        if(!graph.containsEdge(srcNodeExit, targetNodeReturn))
          addEdge(srcNodeExit, targetNodeReturn, EdgeType.TRANSFER)
      case _ =>
    }
    
    met.retVar match {
      case Some(retv) =>
        val targetNode = getNode(pi, srcContext.copy)
        val srcNode = getNode(retv, targetContext.copy)
        worklist += srcNode
        if(!graph.containsEdge(srcNode, targetNode))
          addEdge(srcNode, targetNode, EdgeType.TRANSFER)
      case None =>
    }
  }
  
  def extendGraph(met : Point with Proc, pi : Point with Invoke, srcContext : Context) = {
    breakPiEdges(pi, met.accessTyp, srcContext)
    connectCallEdges(met, pi, srcContext)
  }
  
  def updateContext(callerContext : Context) = {
    this.nodes.foreach{
      node =>
        node.getContext.updateContext(callerContext)
    }
  }
  
  /**
   * This is the recv bar method in original algo
   */
  def recvInverse(n : Node) : Option[Point with Invoke] = {
    n.point match{
      case on : PointRecvCall => Some(on.getContainer)
      case _ => None
    }
  }
  
  def getDirectCallee(pi : Point with Invoke) : JawaProcedure = CallHandler.getDirectCalleeProcedure(pi.sig)
  
  def getStaticCallee(pi : Point with Invoke) : JawaProcedure = CallHandler.getStaticCalleeProcedure(pi.sig)
  
  def getSuperCalleeSet(diff : ISet[Instance],
	                 pi : Point with Invoke) : ISet[JawaProcedure] = {
    val calleeSet : MSet[JawaProcedure] = msetEmpty
    diff.foreach{
      d =>
        val p = CallHandler.getSuperCalleeProcedure(pi.sig)
        calleeSet += p
    }
    calleeSet.toSet
  }

  def getVirtualCalleeSet(diff : ISet[Instance],
	                 pi : Point with Invoke) : ISet[JawaProcedure] = {
    val calleeSet : MSet[JawaProcedure] = msetEmpty
    val subSig = Center.getSubSigFromProcSig(pi.sig)
    diff.foreach{
      d =>
        val p = CallHandler.getVirtualCalleeProcedure(d.typ, subSig)
        calleeSet += p
    }
    calleeSet.toSet
  }
  
  def getNodeOrElse(p : Point, context : Context) : Node = {
    if(!nodeExists(p, context)) addNode(p, context)
    else getNode(p, context)
//    p match {
//      case pal : PointArrayL =>
//        if(!arrayNodeLExists(pal.varName, pal.locationUri, context, pal.dimensions))
//          addArrayNodeL(pal.varName, pal.locationUri, context, pal.dimensions)
//        else getArrayNodeL(pal.varName, pal.locationUri, context, pal.dimensions)
//      case par : PointArrayR =>
//        if(!arrayNodeRExists(par.varName, par.locationUri, context, par.dimensions))
//          addArrayNodeR(par.varName, par.locationUri, context, par.dimensions)
//        else getArrayNodeR(par.varName, par.locationUri, context, par.dimensions)
//      case pgl : PointGlobalL =>
//        if(!globalVarNodeLExists(pgl.varName, pgl.locationUri, context))
//        	addGlobalVarNodeL(pgl.varName, pgl.locationUri, context)
//        else getGlobalVarNodeL(pgl.varName, pgl.locationUri, context)
//      case pgr : PointGlobalR =>
//        if(!globalVarNodeRExists(pgr.varName, pgr.locationUri, context))
//          addGlobalVarNodeR(pgr.varName, pgr.locationUri, context)
//        else getGlobalVarNodeR(pgr.varName, pgr.locationUri, context)
//      case pbl : PointBaseL =>
//        if(!fieldBaseNodeLExists(pbl.varName, pbl.locationUri, context))
//          addFieldBaseNodeL(pbl.varName, pbl.locationUri, context)
//        else getFieldBaseNodeL(pbl.varName, pbl.locationUri, context)  
//      case pbr : PointBaseR =>
//        if(!fieldBaseNodeRExists(pbr.varName, pbr.locationUri, context))
//          addFieldBaseNodeR(pbr.varName, pbr.locationUri, context)
//        else getFieldBaseNodeR(pbr.varName, pbr.locationUri, context)
//      case pfl : PointFieldL =>
//        if(!fieldNodeExists(pfl.basePoint.varName, pfl.varName, pfl.locationUri, context))
//          addFieldNode(pfl.basePoint.varName, pfl.varName, pfl.locationUri, context)
//        else getFieldNode(pfl.basePoint.varName, pfl.varName, pfl.locationUri, context)
//      case pfr : PointFieldR =>
//        if(!fieldNodeExists(pfr.basePoint.varName, pfr.varName, pfr.locationUri, context))
//          addFieldNode(pfr.basePoint.varName, pfr.varName, pfr.locationUri, context)
//        else getFieldNode(pfr.basePoint.varName, pfr.varName, pfr.locationUri, context)
//      case pr : PointRecv_Call =>
//        if(!recvCallNodeExists(pr.varName, pr.locationUri, context, pr.container))
//          addRecvCallNode(pr.varName, pr.locationUri, context, pr.container)
//        else getRecvCallNode(pr.varName, pr.locationUri, context, pr.container)
//      case pr : PointRecv_Return =>
//        if(!recvReturnNodeExists(pr.varName, pr.locationUri, context))
//          addRecvReturnNode(pr.varName, pr.locationUri, context)
//        else getRecvReturnNode(pr.varName, pr.locationUri, context)
////      case pr : PointRecv =>
////        if(!recvNodeExists("recv:" + pr.varName, pr.locationUri, context, pr.container)){
////          val node = addRecvNode("recv:" + pr.varName, pr.locationUri, context, pr.container)
////          node.setProperty(VALUE_SET, fac())
////          node
////        } else getRecvNode("recv:" + pr.varName, pr.locationUri, context, pr.container)
//      case pa : PointArg_Call =>
//        if(!argCallNodeExists(pa.varName, pa.locationUri, context))
//          addArgCallNode(pa.varName, pa.locationUri, context)
//        else getArgCallNode(pa.varName, pa.locationUri, context)
//      case pa : PointArg_Return =>
//        if(!argReturnNodeExists(pa.varName, pa.locationUri, context))
//          addArgReturnNode(pa.varName, pa.locationUri, context)
//        else getArgReturnNode(pa.varName, pa.locationUri, context)
//      case pso : PointStringO =>
//        if(!pointNodeExists("newString:" + pso.varName, pso.locationUri, context))
//          addPointNode("newString:" + pso.varName, pso.locationUri, context)
//        else getPointNode("newString:" + pso.varName, pso.locationUri, context)
//      case po : PointO =>
//        if(!pointNodeExists("new:" + po.varName, po.locationUri, context))
//          addPointNode("new:" + po.varName, po.locationUri, context)
//        else getPointNode("new:" + po.varName, po.locationUri, context)
//      case pi : PointI =>
//        if(!invokeNodeExists(pi.varName, pi.locationUri, context, pi))
//          addInvokeNode(pi.varName, pi.locationUri, context, pi)
//        else getInvokeNode(pi.varName, pi.locationUri, context, pi)
//      case pwi : PointWithIndex =>
//        if(!pointNodeExists(pwi.varName, pwi.locationUri, context))
//          addPointNode(pwi.varName, pwi.locationUri, context)
//        else getPointNode(pwi.varName, pwi.locationUri, context)
//      case pr : PointThis_Entry =>
//        if(!pointNodeExists("this_Entry:" + pr.varName, pr.identifier, context))
//          addPointNode("this_Entry:" + pr.varName, pr.identifier, context)
//        else getPointNode("this_Entry:" + pr.varName, pr.identifier, context)
//      case pr : PointThis_Exit =>
//        if(!pointNodeExists("this_Exit:" + pr.varName, pr.identifier, context))
//          addPointNode("this_Exit:" + pr.varName, pr.identifier, context)
//        else getPointNode("this_Exit:" + pr.varName, pr.identifier, context)
//      case pr : PointThis =>
//        if(!pointNodeExists("this:" + pr.varName, pr.identifier, context))
//          addPointNode("this:" + pr.varName, pr.identifier, context)
//        else getPointNode("this:" + pr.varName, pr.identifier, context)
//      case pa : PointParam_Entry =>
//        if(!pointNodeExists("param_Entry:" + pa.varName, pa.identifier, context))
//          addPointNode("param_Entry:" + pa.varName, pa.identifier, context)
//        else getPointNode("param_Entry:" + pa.varName, pa.identifier, context)
//      case pa : PointParam_Exit =>
//        if(!pointNodeExists("param_Exit:" + pa.varName, pa.identifier, context))
//          addPointNode("param_Exit:" + pa.varName, pa.identifier, context)
//        else getPointNode("param_Exit:" + pa.varName, pa.identifier, context)
//      case pa : PointParam =>
//        if(!pointNodeExists("param:" + pa.varName, pa.identifier, context))
//          addPointNode("param:" + pa.varName, pa.identifier, context)
//        else getPointNode("param:" + pa.varName, pa.identifier, context)
//      case pr : PointRNoIndex =>
//        if(!pointNodeExists(pr.varName, pr.identifier, context))
//          addPointNode(pr.varName, pr.identifier, context)
//        else getPointNode(pr.varName, pr.identifier, context)
//      case pp : PointProc =>
//        if(!pointNodeExists(pp.pSig, pp.getLoc, context))
//          addPointNode(pp.pSig, pp.getLoc, context)
//        else getPointNode(pp.pSig, pp.getLoc, context)
//    }
  }
  
  def nodeExists(point : Point, context : Context) : Boolean = {
    graph.containsVertex(newNode(point, context).asInstanceOf[Node])
  }
//  def entryNodeExists(procPoint : PointProc, context : Context) : Boolean = {
//    graph.containsVertex(newEntryNode(procPoint, context).asInstanceOf[Node])
//  }
//  def exitNodeExists(procPoint : PointProc, context : Context) : Boolean = {
//    graph.containsVertex(newExitNode(procPoint, context).asInstanceOf[Node])
//  }
//  def callNodeExists(pi : Point with Loc with Invoke, context : Context) : Boolean = {
//    graph.containsVertex(newCallNode(pi, context).asInstanceOf[Node])
//  }
//  def returnNodeExists(pi : Point with Loc with Invoke, context : Context) : Boolean = {
//    graph.containsVertex(newReturnNode(pi, context).asInstanceOf[Node])
//  }
//  def arrayNodeLExists(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) : Boolean = {
//    graph.containsVertex(newArrayNodeL(uri, loc, context, dimensions).asInstanceOf[Node])
//  }
//  
//  def arrayNodeRExists(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) : Boolean = {
//    graph.containsVertex(newArrayNodeR(uri, loc, context, dimensions).asInstanceOf[Node])
//  }
//  
//  def globalVarNodeLExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
//    graph.containsVertex(newGlobalVarNodeL(uri, loc, context).asInstanceOf[Node])
//  }
//  
//  def globalVarNodeRExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
//    graph.containsVertex(newGlobalVarNodeR(uri, loc, context).asInstanceOf[Node])
//  }
//  
//  def fieldBaseNodeLExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
//    graph.containsVertex(newFieldBaseNodeL(uri, loc, context).asInstanceOf[Node])
//  }
//  
//  def fieldBaseNodeRExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
//    graph.containsVertex(newFieldBaseNodeR(uri, loc, context).asInstanceOf[Node])
//  }
//  
//  def fieldNodeExists(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
//    graph.containsVertex(newFieldNode(baseName, fieldName, loc, context).asInstanceOf[Node])
//  }
//  
//  def invokeNodeExists(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) : Boolean = {
//    graph.containsVertex(newInvokeNode(uri, loc, context, pi).asInstanceOf[Node])
//  }
//  
//  def recvCallNodeExists(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) : Boolean = {
//    graph.containsVertex(newRecvCallNode(uri, loc, context, pi).asInstanceOf[Node])
//  }
//  
//  def recvReturnNodeExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
//    graph.containsVertex(newRecvReturnNode(uri, loc, context).asInstanceOf[Node])
//  }
//  
//  def argCallNodeExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
//    graph.containsVertex(newArgCallNode(uri, loc, context).asInstanceOf[Node])
//  }
//  
//  def argReturnNodeExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
//    graph.containsVertex(newArgReturnNode(uri, loc, context).asInstanceOf[Node])
//  }
//  
//  def pointNodeExists(uri : ResourceUri, loc : ResourceUri, context : Context) : Boolean = {
//    graph.containsVertex(newPointNode(uri, loc, context).asInstanceOf[Node])
//  }
  
  def addNode(point : Point, context : Context) : Node = {
    val node = newNode(point, context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
//  def addArrayNodeL(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) : Node = {
//    val node = newArrayNodeL(uri, loc, context, dimensions).asInstanceOf[Node]
//    val n =
//      if (pool.contains(node)) pool(node)
//      else {
//        pl += (node -> node)
//        node
//      }
//    graph.addVertex(n)
//    n
//  }
//  
//  def addArrayNodeR(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) : Node = {
//    val node = newArrayNodeR(uri, loc, context, dimensions).asInstanceOf[Node]
//    val n =
//      if (pool.contains(node)) pool(node)
//      else {
//        pl += (node -> node)
//        node
//      }
//    graph.addVertex(n)
//    n
//  }
//  
//  def addGlobalVarNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
//    val node = newGlobalVarNodeL(uri, loc, context).asInstanceOf[Node]
//    val n =
//      if (pool.contains(node)) pool(node)
//      else {
//        pl += (node -> node)
//        node
//      }
//    graph.addVertex(n)
//    n
//  }
//  
//  def addGlobalVarNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
//    val node = newGlobalVarNodeR(uri, loc, context).asInstanceOf[Node]
//    val n =
//      if (pool.contains(node)) pool(node)
//      else {
//        pl += (node -> node)
//        node
//      }
//    graph.addVertex(n)
//    n
//  }
//  
//  def addFieldBaseNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
//    val node = newFieldBaseNodeL(uri, loc, context).asInstanceOf[Node]
//    val n =
//      if (pool.contains(node)) pool(node)
//      else {
//        pl += (node -> node)
//        node
//      }
//    graph.addVertex(n)
//    n
//  }
//  
//  def addFieldBaseNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
//    val node = newFieldBaseNodeR(uri, loc, context).asInstanceOf[Node]
//    val n =
//      if (pool.contains(node)) pool(node)
//      else {
//        pl += (node -> node)
//        node
//      }
//    graph.addVertex(n)
//    n
//  }
//  
//  def addFieldNode(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri, context : Context) : Node = {
//    val node = newFieldNode(baseName, fieldName, loc, context).asInstanceOf[Node]
//    val n =
//      if (pool.contains(node)) pool(node)
//      else {
//        pl += (node -> node)
//        node
//      }
//    graph.addVertex(n)
//    n
//  }
//  
//  def addInvokeNode(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) : Node = {
//    val node = newInvokeNode(uri, loc, context, pi).asInstanceOf[Node]
//    val n =
//      if (pool.contains(node)) pool(node)
//      else {
//        pl += (node -> node)
//        node
//      }
//    graph.addVertex(n)
//    n
//  }
//  
//  def addRecvCallNode(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) : Node = {
//    val node = newRecvCallNode(uri, loc, context, pi).asInstanceOf[Node]
//    val n =
//      if (pool.contains(node)) pool(node)
//      else {
//        pl += (node -> node)
//        node
//      }
//    graph.addVertex(n)
//    n
//  }
//  
//  def addRecvReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
//    val node = newRecvReturnNode(uri, loc, context).asInstanceOf[Node]
//    val n =
//      if (pool.contains(node)) pool(node)
//      else {
//        pl += (node -> node)
//        node
//      }
//    graph.addVertex(n)
//    n
//  }
//  
//  def addArgCallNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
//    val node = newArgCallNode(uri, loc, context).asInstanceOf[Node]
//    val n =
//      if (pool.contains(node)) pool(node)
//      else {
//        pl += (node -> node)
//        node
//      }
//    graph.addVertex(n)
//    n
//  }
//  
//  def addArgReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
//    val node = newArgReturnNode(uri, loc, context).asInstanceOf[Node]
//    val n =
//      if (pool.contains(node)) pool(node)
//      else {
//        pl += (node -> node)
//        node
//      }
//    graph.addVertex(n)
//    n
//  }
//
//  def addPointNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node = {
//    val node = newPointNode(uri, loc, context).asInstanceOf[Node]
//    val n =
//      if (pool.contains(node)) pool(node)
//      else {
//        pl += (node -> node)
//        node
//      }
//    graph.addVertex(n)
//    n
//  }
  
  def getNode(point : Point, context : Context) : Node = pool(newNode(point, context))
//  def getEntryNode(procPoint : PointProc, context : Context) = pool(newEntryNode(procPoint, context))
//  def getExitNode(procPoint : PointProc, context : Context) = pool(newExitNode(procPoint, context))
//  def getCallNode(pi : Point with Loc with Invoke, context : Context) = pool(newCallNode(pi, context))
//  def getReturnNode(pi : Point with Loc with Invoke, context : Context) = pool(newReturnNode(pi, context))
//  def getArrayNodeL(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) : Node =
//    pool(newArrayNodeL(uri, loc, context, dimensions))
//    
//  def getArrayNodeR(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) : Node =
//    pool(newArrayNodeR(uri, loc, context, dimensions))
//    
//  def getGlobalVarNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
//    pool(newGlobalVarNodeL(uri, loc, context))
//    
//  def getGlobalVarNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
//    pool(newGlobalVarNodeR(uri, loc, context))
//    
//  def getFieldBaseNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
//    pool(newFieldBaseNodeL(uri, loc, context))
//    
//  def getFieldBaseNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
//    pool(newFieldBaseNodeR(uri, loc, context))
//    
//  def getFieldNode(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri, context : Context) : Node =
//    pool(newFieldNode(baseName, fieldName, loc, context))
//    
//  def getInvokeNode(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) : Node =
//    pool(newInvokeNode(uri, loc, context, pi))
//    
//  def getRecvCallNode(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) : Node =
//    pool(newRecvCallNode(uri, loc, context, pi))
//    
//  def getRecvReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
//    pool(newRecvReturnNode(uri, loc, context))
//    
//  def getArgCallNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
//    pool(newArgCallNode(uri, loc, context))
//    
//  def getArgReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
//    pool(newArgReturnNode(uri, loc, context))
//
//  def getPointNode(uri : ResourceUri, loc : ResourceUri, context : Context) : Node =
//    pool(newPointNode(uri, loc, context))
    
//  def getNode(p : Point, context : Context) : Node = {
//    p match {
//      case pal : PointArrayL =>
//        getArrayNodeL(pal.varName, pal.locationUri, context, pal.dimensions)
//      case par : PointArrayR =>
//        getArrayNodeR(par.varName, par.locationUri, context, par.dimensions)
//      case pgl : PointGlobalL =>
//        getGlobalVarNodeL(pgl.varName, pgl.locationUri, context)
//      case pgr : PointGlobalR =>
//        getGlobalVarNodeR(pgr.varName, pgr.locationUri, context)
//      case pbl : PointBaseL =>
//        getFieldBaseNodeL(pbl.varName, pbl.locationUri, context)
//      case pbr : PointBaseR =>
//        getFieldBaseNodeR(pbr.varName, pbr.locationUri, context)
//      case pfl : PointFieldL =>
//        getFieldNode(pfl.basePoint.varName, pfl.varName, pfl.locationUri, context)
//      case pfr : PointFieldR =>
//        getFieldNode(pfr.basePoint.varName, pfr.varName, pfr.locationUri, context)
//      case pr : PointRecv_Call =>
//        getRecvCallNode(pr.varName, pr.locationUri, context, pr.container)
//      case pr : PointRecv_Return =>
//        getRecvReturnNode(pr.varName, pr.locationUri, context)
////      case pr : PointRecv =>
////        getRecvNode("recv:" + pr.varName, pr.locationUri, context, pr.container)
//      case pa : PointArg_Call =>
//        getArgCallNode(pa.varName, pa.locationUri, context)
//      case pa : PointArg_Return =>
//        getArgReturnNode(pa.varName, pa.locationUri, context)
//      case po : PointStringO =>
//        getPointNode("newString:" + po.varName, po.locationUri, context)
//      case po : PointO =>
//        getPointNode("new:" + po.varName, po.locationUri, context)
//      case pi : PointI =>
//        getInvokeNode(pi.varName, pi.locationUri, context, pi)
//      case pwi : PointWithIndex =>
//        getPointNode(pwi.varName, pwi.locationUri, context)
//      case pr : PointThis_Entry =>
//        getPointNode("this_Entry:" + pr.varName, pr.identifier, context)
//      case pr : PointThis_Exit =>
//        getPointNode("this_Exit:" + pr.varName, pr.identifier, context)
//      case pr : PointThis =>
//        getPointNode("this:" + pr.varName, pr.identifier, context)
//      case pa : PointParam_Entry =>
//        getPointNode("param_Entry:" + pa.varName, pa.identifier, context)
//      case pa : PointParam_Exit =>
//        getPointNode("param_Exit:" + pa.varName, pa.identifier, context)
//      case pa : PointParam =>
//        getPointNode("param:" + pa.varName, pa.identifier, context)
//      case pri : PointRNoIndex =>
//        getPointNode(pri.varName, pri.identifier, context)
//      case pp : PointProc =>
//        getPointNode(pp.pSig, pp.getLoc, context)
//    }
//  }
  
  protected def newNode(point : Point, context : Context) = PtaNode(point, context)
//  protected def newEntryNode(procPoint : PointProc, context : Context) = PtaEntryNode(procPoint, context)
//  protected def newExitNode(procPoint : PointProc, context : Context) = PtaExitNode(procPoint, context)
//  protected def newCallNode(pi : Point with Loc with Invoke, context : Context) = PtaCallNode(pi, context)
//  protected def newReturnNode(pi : Point with Loc with Invoke, context : Context) = PtaReturnNode(pi, context)
//  protected def newFieldNode(baseName : ResourceUri, fieldName : ResourceUri, loc : ResourceUri, context : Context) =
//    PtaFieldNode(baseName, fieldName, loc, context)
//  
//  protected def newFieldBaseNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) =
//    PtaFieldBaseNodeL(uri, loc, context)
//    
//  protected def newFieldBaseNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) =
//    PtaFieldBaseNodeR(uri, loc, context)
//    
//  protected def newArrayNodeL(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) =
//    PtaArrayNodeL(uri, loc, context, dimensions)
//    
//  protected def newArrayNodeR(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) =
//    PtaArrayNodeR(uri, loc, context, dimensions)
//    
//  protected def newGlobalVarNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) =
//    PtaGlobalVarNodeL(uri, loc, context)
//    
//  protected def newGlobalVarNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) =
//    PtaGlobalVarNodeR(uri, loc, context)
//
//  protected def newInvokeNode(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) = {
//    val n = PtaInvokeNode(uri, loc, context, pi.typ)
//    n.setPI(pi)
//    n
//  }
//    
//  protected def newRecvCallNode(uri : ResourceUri, loc : ResourceUri, context : Context, pi : PointI) = {
//    val n = PtaRecvCallNode(uri, loc, context)
//    n.setPI(pi)
//    n
//  }
//  
//  protected def newRecvReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) = 
//    PtaRecvReturnNode(uri, loc, context)
//    
//  protected def newArgCallNode(uri : ResourceUri, loc : ResourceUri, context : Context) = 
//    PtaArgCallNode(uri, loc, context)
//
//  protected def newArgReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) = 
//    PtaArgReturnNode(uri, loc, context)
//    
//  protected def newPointNode(uri : ResourceUri, loc : ResourceUri, context : Context) =
//    PtaPointNode(uri, loc, context)
    
  override def toString = {
      val sb = new StringBuilder("PAG\n")

      for (n <- nodes)
        for (m <- successors(n)) {
          for (e <- getEdges(n, m)) {
            sb.append("%s -> %s\n".format(n, m))
          }
        }

      sb.append("\n")

      sb.toString
  }
}

final case class PtaNode(point : Point, context : Context) extends InterProceduralNode(context) {
  def getSlots(ptaresult : PTAResult) : ISet[Slot] = {
    point match {
      case pao : PointArrayO =>
        Set(InstanceSlot(PTAInstance(new NormalType(pao.obj, pao.dimensions), context.copy)))
      case po : PointO =>
        Set(InstanceSlot(PTAInstance(new NormalType(po.obj), context.copy)))
      case pso : PointStringO =>
        Set(InstanceSlot(PTAConcreteStringInstance(pso.text, context.copy)))
      case gla : Point with Loc with Global with Array =>
        val pts = ptaresult.pointsToSet(VarSlot(gla.globalSig), context)
        pts.map{
          ins =>
            ArraySlot(ins)
        }
      case glo : Point with Loc with Global =>
        Set(VarSlot(glo.globalSig))
      case arr : PointArrayL =>
        val pts = ptaresult.pointsToSet(VarSlot(arr.arrayname), context)
        pts.map{
          ins =>
            ArraySlot(ins)
        }
      case arr : PointArrayR =>
        val pts = ptaresult.pointsToSet(VarSlot(arr.arrayname), context)
        pts.map{
          ins =>
            ArraySlot(ins)
        }
      case fie : Point with Loc with Field =>
        val pts = ptaresult.pointsToSet(VarSlot(fie.baseP.baseName), context)
        pts.map{
          ins =>
            FieldSlot(ins, fie.fieldName)
        }
      case bas : Point with Loc with Base =>
        Set(VarSlot(bas.baseName))
      case pl : PointL =>
        Set(VarSlot(pl.varname))
      case pr : PointR =>
        Set(VarSlot(pr.varname))
      case pla : Point with Loc with Arg =>
        Set(VarSlot(pla.argName))
      case pop : Point with Param =>
        Set(VarSlot(pop.paramName))
      case inp : Point with Invoke =>
        Set(InvokeSlot(inp.sig, inp.invokeTyp))
      case _ => throw new RuntimeException("No slot for such pta node: " + point + "@" + context)
    }
  }
}

//final case class PtaLocNode(point : Point, context : Context) extends PtaNode(point, context)
//
//final case class PtaEntryNode(procPoint : Point with Proc, context : Context) extends PtaNode(procPoint, context)
//
//final case class PtaExitNode(procPoint : Point with Proc, context : Context) extends PtaNode(procPoint, context)
//
//final case class PtaCallNode(pi : Point with Loc with Invoke, context : Context) extends PtaNode(pi, context)
//
//final case class PtaReturnNode(pi : Point with Loc with Invoke, context : Context) extends PtaNode(pi, context)

//final case class PtaProcNode(procPoint : Point with Proc, context : Context) extends PtaNode(procPoint, context)
//
//final case class PtaNormalNode(point : Point, context : Context) extends PtaNode(point, context)
//
///**
// * Node type for invocation point.
// */
//final case class PtaInvokeNode(uri : ResourceUri, loc : ResourceUri, context : Context, typ : String) extends PtaNode(loc, context) {
//  private var pi : PointI = null
//  def setPI(pi : PointI) = this.pi = pi
//  def getPI = this.pi
//  override def toString = typ + "_invoke:" + uri + "@" + context.toString()
//}
//
///**
// * Node type for receive call point.
// */
//final case class PtaRecvCallNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
//  private var pi : PointI = null
//  def setPI(pi : PointI) = this.pi = pi
//  def getPI = this.pi
//  override def toString = "recv_Call:" + uri + "@" + context.toString()
//}
//
///**
// * Node type for receive return point.
// */
//final case class PtaRecvReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
//  override def toString = "recv_Return:" + uri + "@" + context.toString()
//}
//
///**
// * Node type for receive call point.
// */
//final case class PtaArgCallNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
//  override def toString = "arg_Call:" + uri + "@" + context.toString()
//}
//
///**
// * Node type for receive return point.
// */
//final case class PtaArgReturnNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
//  override def toString = "arg_Return:" + uri + "@" + context.toString()
//}
//
///**
// * Node type for base part of field access to store hidden edge for it's fieldNode.
// */
//abstract class PtaFieldBaseNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
//  var fieldNode : PtaFieldNode = null
//  override def toString = "base:" + uri + "@" + context.toString()
//}
//
///**
// * Node type for base part of field access to store hidden edge for it's fieldNode.
// */
//final case class PtaFieldBaseNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaFieldBaseNode(uri, loc, context) {
//  override def toString = "base_lhs:" + uri + "@" + context.toString()
//}
//
///**
// * Node type for base part of field access to store hidden edge for it's fieldNode.
// */
//final case class PtaFieldBaseNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaFieldBaseNode(uri, loc, context) {
//  override def toString = "base_rhs:" + uri + "@" + context.toString()
//}
//
///**
// * Node type for field access to store hidden edge for it's baseNode.
// */
//final case class PtaFieldNode(baseName : ResourceUri, fieldSig : String, loc : ResourceUri, context : Context) extends PtaNode(loc, context) {
//  var baseNode : PtaFieldBaseNode = null
//  override def toString = "field:" + baseName + "." + fieldSig + "@" + context.toString()
//}
//
///**
// * Node type for array variable.
// */
//abstract class PtaArrayNode(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) extends PtaNode(loc, context){
//  def dimen = dimensions
//}
//
///**
// * Node type for array variable in lhs.
// */
//final case class PtaArrayNodeL(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) extends PtaArrayNode(uri, loc, context, dimensions) {
//  override def toString = "array_lhs:" + uri + "@" + context.toString()
//}
//
///**
// * Node type for array variable in rhs.
// */
//final case class PtaArrayNodeR(uri : ResourceUri, loc : ResourceUri, context : Context, dimensions : Int) extends PtaArrayNode(uri,loc, context, dimensions) {
//  override def toString = "array_rhs:" + uri + "@" + context.toString()
//}
//
//
///**
// * Node type for global variable.
// */
//abstract class PtaGlobalVarNode(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaNode(loc, context){
//  def name = uri
//}
//
///**
// * Node type for global variable in lhs.
// */
//final case class PtaGlobalVarNodeL(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaGlobalVarNode(uri, loc, context) {
//  override def toString = "global_lhs:" + uri.replaceAll("@@", "") + "@" + context.toString()
//}
//
///**
// * Node type for global variable in rhs.
// */
//final case class PtaGlobalVarNodeR(uri : ResourceUri, loc : ResourceUri, context : Context) extends PtaGlobalVarNode(uri,loc, context) {
//  override def toString = "global_rhs:" + uri.replaceAll("@@", "") + "@" + context.toString()
//}