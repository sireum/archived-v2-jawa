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
import org.sireum.jawa.alir.interProcedural.InstanceCallee
import org.sireum.jawa.alir.interProcedural.StaticCallee
import org.sireum.jawa.alir.pta.StaticFieldSlot
import org.sireum.jawa.alir.pta.PTAInstance
import org.sireum.jawa.alir.pta.ClassInstance

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class PointsToMap extends PTAResult {
  
  final val TITLE = "PointsToMap"
  
  /**
   * e.g. L0: p = q; L1:  r = p; transfer means p@L0 -> p@L1
   */
  def transferPointsToSet(n1: PtaNode, n2: PtaNode) = {
    n2.getSlots(this) foreach{
      addInstances(_, n2.getContext.copy, pointsToSet(n1))
    }
  }
  
  /**
   * e.g. L0: p = q; L1:  r = p; transfer means p@L0 -> p@L1
   */
  def transferPointsToSet(n: PtaNode, d: ISet[Instance]) = {
    n.getSlots(this) foreach{
      addInstances(_, n.getContext.copy, d)
    }
  }
  
  /**
   * n1 -> n2 or n1.f -> n2 or n1[] -> n2, n1 -> n2.f, n1 -> n2[]
   */
  def propagatePointsToSet(n1: PtaNode, n2: PtaNode) = {
    n2.getSlots(this) foreach {
      slot =>
        slot match {
          case arr: ArraySlot =>
            addInstances(arr, n2.getContext, pointsToSet(n1))
          case fie: FieldSlot =>
            addInstances(fie, n2.getContext, pointsToSet(n1))
          case _ =>
            setInstances(slot, n2.getContext, pointsToSet(n1))
        }
    }
  }
  
  /**
   * n or n.f or n[] or @@n
   */
  def pointsToSet(n: PtaNode): ISet[Instance] = {
    val slots = n.getSlots(this)
    if(!slots.isEmpty){
      slots.map {
        s =>
          pointsToSet(s, n.getContext)
      }.reduce(iunion[Instance])
    } else isetEmpty
  }

  def isDiff(n1: PtaNode, n2: PtaNode): Boolean = {
    pointsToSet(n1) != pointsToSet(n2)
  }
  
//  def isThisDiff(n1: PtaNode, n2: PtaNode): Boolean = {
//    !getThisDiff(n1, n2).isEmpty
//  }
  
  def contained(n1: PtaNode, n2: PtaNode): Boolean = {
    (pointsToSet(n1) -- pointsToSet(n2)).isEmpty
  }
  
  def getDiff(n1: PtaNode, n2: PtaNode): ISet[Instance] = {
    pointsToSet(n1) diff pointsToSet(n2)
  }
  
//  def getThisDiff(n1: PtaNode, n2: PtaNode): ISet[Instance] = {
//    assert(n2.point.isInstanceOf[PointThisEntry])
//    val ptsdiff = pointsToSet(n1) -- pointsToSet(n2)
//    val thisent = n2.point.asInstanceOf[PointThisEntry]
//    val calleeSubSig = thisent.ownerSig.getSubSignature
//    val thiscls = Center.resolveClass(thisent.paramTyp.name, Center.ResolveLevel.HIERARCHY)
//    ptsdiff.filter { 
//      ins => 
//        val inscls = Center.resolveClass(ins.typ.name, Center.ResolveLevel.HIERARCHY)
//        var res: Boolean = false
//        var tmpRec = inscls
//        import scala.util.control.Breaks._
//        breakable{
//          while(tmpRec.hasSuperClass){
//            if(tmpRec == thiscls){
//              res = true
//              break
//            }
//            else if(tmpRec.declaresMethod(calleeSubSig)){
//              res = false
//              break
//            }
//            else tmpRec = tmpRec.getSuperClass
//          }
//        }
//        if(tmpRec == inscls) res = true
//        else {
//          err_msg_detail(TITLE, "Given inscls: " + inscls + " and thiscls: " + thiscls + " is not in the Same hierachy.")
//          res = false
//        }
//        res
//    }
//  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class PointerAssignmentGraph[Node <: PtaNode]
  extends InterProceduralGraph[Node]
  with PAGConstraint{
  self=>
  private final val TITLE = "PointerAssignmentGraph"
  val pointsToMap = new PointsToMap
  
  private val processed: MMap[(JawaMethod, Context), Point with Method] = new HashMap[(JawaMethod, Context), Point with Method] with SynchronizedMap[(JawaMethod, Context), Point with Method]
  
  def isProcessed(proc: JawaMethod, callerContext: Context): Boolean = processed.contains(proc, callerContext)
  
  def getPointMethod(proc: JawaMethod, callerContext: Context): Point with Method =  processed(proc, callerContext)
  
  def addProcessed(jp: JawaMethod, c: Context, ps: Set[Point]) = {
    ps.foreach{
      p =>
        if(p.isInstanceOf[Point with Method])
          this.processed += ((jp, c) -> p.asInstanceOf[Point with Method])
    }
  }
  
  def getProcessed = this.processed
  
  private var newNodes: Set[Node] = isetEmpty
  private var newEdges: Set[Edge] = isetEmpty
  
  final case class PTACallee(callee: JawaMethod, pi: Point with Invoke, node: Node) extends Callee
  
  def processStaticCall(global: Global): ISet[PTACallee] = {
    val staticCallees = msetEmpty[PTACallee]
    newNodes.foreach{
      node =>
        if(node.point.isInstanceOf[Point with Invoke]){
          val pi = node.point.asInstanceOf[Point with Invoke]
          if(pi.invokeTyp.equals("static")){
            val callee = getStaticCallee(global, pi)
            staticCallees += PTACallee(callee.callee, pi, node)
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
  
  def addEdge(source: Node, target: Node, typ: EdgeType.Value): Edge = {
    val edge = graph.addEdge(getNode(source), getNode(target))
    edge.setProperty(EDGE_TYPE, typ)
    edge
  }
  
  def getEdgeType(edge: Edge): EdgeType.Value = {
    assume(edge.propertyMap.contains(EDGE_TYPE))
    edge.getProperty[EdgeType.Value](EDGE_TYPE)
  }
  
  final val EDGE_TYPE = "EdgeType"
  final val PARAM_NUM = "ParamNumber"

  /**
   * represents max number of strings in the strings set of a StringInstance
   */
//  final val K_STRING: Int = 5
  
  
  final val worklist: MList[Node] = mlistEmpty
    
  /**
   * combine two pags into one.
   */ 
  def combinePags(pag2: PointerAssignmentGraph[Node]) = {
    pl ++= pag2.pool
    pag2.nodes.foreach(
      node=>{
        addNode(node)
      }
    )
    pag2.edges.foreach(
      e=>{
        addEdge(e)
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
  def constructGraph(ap: JawaMethod, ps: Set[Point], callerContext: Context) = {
    addProcessed(ap, callerContext.copy, ps)
    ps.foreach{
      p =>
        newNodes ++= collectNodes(ap.getSignature, p, callerContext.copy)
    }
    ps.foreach{
      p =>
        val cfg = JawaAlirInfoProvider.getCfg(ap)
        val rda = JawaAlirInfoProvider.getRda(ap, cfg, false)
        val constraintMap = applyConstraint(p, ps, cfg, rda)
        newEdges ++= buildingEdges(constraintMap, ap.getSignature, callerContext.copy)
    }
  }
  

  def collectNodes(pSig: Signature, p: Point, callerContext: Context): Set[Node] = {
    var nodes: Set[Node] = Set()
    val context = callerContext.copy
    p match {
      case lp: Point with Loc => context.setContext(pSig, lp.loc)
      case _ => context.setContext(pSig, p.ownerSig.signature)
    }
    
    p match {
      case cp: PointCall =>
        val lhsopt = cp.lhsOpt
        val rhs = cp.rhs
        lhsopt foreach { nodes += getNodeOrElse(_, context.copy) }
        val rhsNode = getNodeOrElse(rhs, context.copy)
        nodes += rhsNode
        rhs match {
          case pi: Point with Invoke =>
            pi match {
              case vp: Point with Invoke with Dynamic =>
                nodes += getNodeOrElse(vp.recvPCall, context.copy)
                nodes += getNodeOrElse(vp.recvPReturn, context.copy)
              case _ =>
                worklist += rhsNode
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
        }
      case asmtP: PointAsmt =>
        val lhs = asmtP.lhs
        val rhs = asmtP.rhs
        val lhsNode = getNodeOrElse(lhs, context.copy)
        nodes += lhsNode
        val rhsNode = getNodeOrElse(rhs, context.copy)
        nodes += rhsNode
        lhs match {
          case pfl: PointFieldL =>
            val fieldNode = getNodeOrElse(pfl, context.copy)
            nodes += fieldNode
            val baseNode = getNodeOrElse(pfl.baseP, context.copy)
            nodes += baseNode
//            baseNode.asInstanceOf[PtaFieldBaseNode].fieldNode = fieldNode.asInstanceOf[PtaFieldNode]
//            fieldNode.asInstanceOf[PtaFieldNode].baseNode = baseNode.asInstanceOf[PtaFieldBaseNode]
          case _ =>
        }
        rhs match {
          case pgr: PointStaticFieldR =>
            val globalVarNode = getNodeOrElse(pgr, context.copy)
            nodes += globalVarNode
          case pfr: PointFieldR =>
            val fieldNode = getNodeOrElse(pfr, context.copy)
            nodes += fieldNode
            val baseNode = getNodeOrElse(pfr.baseP, context.copy)
            nodes += baseNode
//            baseNode.asInstanceOf[PtaFieldBaseNode].fieldNode = fieldNode.asInstanceOf[PtaFieldNode]
//            fieldNode.asInstanceOf[PtaFieldNode].baseNode = baseNode.asInstanceOf[PtaFieldBaseNode]
          case pcr: PointClassR =>
            val ins = ClassInstance(pcr.classtyp, context.copy)
            pointsToMap.addInstance(InstanceSlot(ins), context.copy, ins)
          case pso: PointStringO =>
            val ins = PTAConcreteStringInstance(pso.text, context.copy)
            pointsToMap.addInstance(InstanceSlot(ins), context.copy, ins)
          case pao: PointArrayO =>
            val ins = PTAInstance(ObjectType(pao.obj, pao.dimensions), context.copy, false)
            pointsToMap.addInstance(InstanceSlot(ins), context.copy, ins)
          case po: PointO =>
            val ins = PTAInstance(new ObjectType(po.obj), context.copy, false)
            pointsToMap.addInstance(InstanceSlot(ins), context.copy, ins)
          case _ =>
        }
      case procP: Point with Method =>
        procP match {
          case vp: Point with Method with Virtual => 
            nodes += getNodeOrElse(vp.thisPEntry, context.copy)
            nodes += getNodeOrElse(vp.thisPExit, context.copy)
          case _ =>
        }
        procP.retVar match {
          case Some(rev) =>
            nodes += getNodeOrElse(rev, context.copy)
          case None =>
        }
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
      case retP: PointRet =>
        nodes += getNodeOrElse(retP, context.copy)
      case _ =>
    }
    nodes
  }
  
  def buildingEdges(map: MMap[EdgeType.Value, MMap[Point, MSet[Point]]], pSig: Signature, context: Context): Set[Edge] = {
    var edges: Set[Edge] = isetEmpty
    map.foreach{
      case(typ, edgeMap) =>
        edgeMap.foreach{
          case(src, dsts) =>
            val s = context.copy
            src match {
              case lp: Point with Loc => s.setContext(pSig, lp.loc)
              case _ => s.setContext(pSig, src.ownerSig.signature)
            }
            val srcNode = getNode(src, s)
            dsts.foreach{
              dst => 
                val t = context.copy
                dst match {
                  case lp: Point with Loc => t.setContext(pSig, lp.loc)
                  case _ => t.setContext(pSig, dst.ownerSig.signature)
                }
                val targetNode = getNode(dst, t)
                if(!graph.containsEdge(srcNode, targetNode))
                  edges += addEdge(srcNode, targetNode, typ)
            }
        }
    }
    edges
  }
  
  def breakPiEdges(pi: Point with Invoke, calleeAccessTyp: String, srcContext: Context) = {
    pi match {
      case vp: Point with Invoke with Dynamic =>
        if(calleeAccessTyp != null){
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
  
  private def connectCallEdges(met: Point with Method, pi: Point with Invoke, srcContext: Context) = {
    val targetContext = srcContext.copy
    targetContext.setContext(met.methodSig, met.ownerSig.signature)
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
      case vp: Point with Method with Virtual =>
        assume(pi.isInstanceOf[PointI])
        val srcNodeCall = getNode(pi.asInstanceOf[PointI].recvPCall, srcContext.copy)
        val targetNodeEntry = getNode(vp.thisPEntry, targetContext.copy)
        worklist += srcNodeCall
        if(!graph.containsEdge(srcNodeCall, targetNodeEntry))
          addEdge(srcNodeCall, targetNodeEntry, EdgeType.THIS_TRANSFER)
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
  
  def extendGraph(met: Point with Method, pi: Point with Invoke, srcContext: Context) = {
    breakPiEdges(pi, met.accessTyp, srcContext)
    connectCallEdges(met, pi, srcContext)
  }
  
  def updateContext(callerContext: Context) = {
    this.nodes.foreach{
      node =>
        node.getContext.updateContext(callerContext)
    }
  }
  
  /**
   * This is the recv bar method in original algo
   */
  def recvInverse(n: Node): Option[Point with Invoke] = {
    n.point match{
      case on: PointRecvCall =>
        Some(on.getContainer)
      case _ => None
    }
  }
  
  def getDirectCallee(
      global: Global,
      diff: ISet[Instance],
      pi: Point with Invoke): ISet[InstanceCallee] = {
    CallHandler.getDirectCalleeMethod(global, pi.sig) match {
      case Some(calleemethod) => diff.map(InstanceCallee(calleemethod, _))
      case None => isetEmpty
    }
  }
  
  def getStaticCallee(global: Global, pi: Point with Invoke): StaticCallee = {
    StaticCallee(CallHandler.getStaticCalleeMethod(global, pi.sig).get)
  }
  
  def getSuperCalleeSet(
      global: Global,
      diff: ISet[Instance],
      pi: Point with Invoke): ISet[InstanceCallee] = {
    val calleeSet: MSet[InstanceCallee] = msetEmpty
    diff.foreach{
      d =>
        val p = CallHandler.getSuperCalleeMethod(global, pi.sig)
        calleeSet ++= p.map(InstanceCallee(_, d))
    }
    calleeSet.toSet
  }

  def getVirtualCalleeSet(
      global: Global,
      diff: ISet[Instance],
      pi: Point with Invoke): ISet[InstanceCallee] = {
    val calleeSet: MSet[InstanceCallee] = msetEmpty
    val subSig = pi.sig.getSubSignature
    diff.foreach{
      d =>
        val p = CallHandler.getVirtualCalleeMethod(global, d.typ, subSig)
        calleeSet ++= p.map(InstanceCallee(_, d))
    }
    calleeSet.toSet
  }
  
  def getNodeOrElse(p: Point, context: Context): Node = {
    if(!nodeExists(p, context)) addNode(p, context)
    else getNode(p, context)
  }
  
  def nodeExists(point: Point, context: Context): Boolean = {
    graph.containsVertex(newNode(point, context).asInstanceOf[Node])
  }
  
  def addNode(point: Point, context: Context): Node = {
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
  
  def getNode(point: Point, context: Context): Node = pool(newNode(point, context))
  
  protected def newNode(point: Point, context: Context) = PtaNode(point, context)

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

final case class PtaNode(point: Point, context: Context) extends InterProceduralNode(context) {
  def getSlots(ptaresult: PTAResult): ISet[Slot] = {
    point match {
      case pao: PointArrayO =>
        Set(InstanceSlot(PTAInstance(new ObjectType(pao.obj, pao.dimensions), context.copy, false)))
      case po: PointO =>
        Set(InstanceSlot(PTAInstance(new ObjectType(po.obj), context.copy, false)))
      case pso: PointStringO =>
        Set(InstanceSlot(PTAConcreteStringInstance(pso.text, context.copy)))
      case gla: Point with Loc with Static_Field with Array =>
        val pts = ptaresult.pointsToSet(StaticFieldSlot(gla.staticFieldFQN), context)
        pts.map{
          ins =>
            ArraySlot(ins)
        }
      case glo: Point with Loc with Static_Field =>
        Set(StaticFieldSlot(glo.staticFieldFQN))
      case arr: PointArrayL =>
        val pts = ptaresult.pointsToSet(VarSlot(arr.arrayname, true, false), context)
        pts.map{
          ins =>
            ArraySlot(ins)
        }
      case arr: PointArrayR =>
        val pts = ptaresult.pointsToSet(VarSlot(arr.arrayname, true, false), context)
        pts.map{
          ins =>
            ArraySlot(ins)
        }
      case fie: Point with Loc with Field =>
        val pts = ptaresult.pointsToSet(VarSlot(fie.baseP.baseName, true, false), context)
        pts.map{
          ins =>
            FieldSlot(ins, fie.fieldName)
        }
      case bas: Point with Loc with Base =>
        Set(VarSlot(bas.baseName, true, false))
      case pl: PointL =>
        Set(VarSlot(pl.varname, false, false))
      case pr: PointR =>
        Set(VarSlot(pr.varname, false, false))
      case pla: Point with Loc with Arg =>
        Set(VarSlot(pla.argName, false, true))
      case pop: Point with Param =>
        Set(VarSlot(pop.paramName, false, false))
      case inp: Point with Invoke =>
        Set(InvokeSlot(inp.sig, inp.invokeTyp))
      case p: PointRet =>
        Set(VarSlot(p.retname, false, false))
      case p: PointMethodRet =>
        Set(VarSlot("ret", false, false))
      case _ => throw new RuntimeException("No slot for such pta node: " + point + "@" + context)
    }
  }
}