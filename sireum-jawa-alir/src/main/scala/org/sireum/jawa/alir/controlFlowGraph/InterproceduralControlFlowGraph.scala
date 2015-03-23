/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.controlFlowGraph

import org.sireum.alir._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.util._
import org.sireum.pilar.ast._
import org.jgrapht.ext.VertexNameProvider
import java.io._
import org.jgrapht.ext.DOTExporter
import dk.brics.automaton._
import org.sireum.jawa.alir.compressedControlFlowGraph.AlirIntraProceduralGraphExtra
import org.sireum.jawa.alir.interProcedural.InterProceduralGraph
import org.sireum.jawa.alir.interProcedural.InterProceduralNode
import org.sireum.jawa.alir.Context
import scala.collection.immutable.BitSet
import scala.collection.mutable.SynchronizedMap
import scala.collection.mutable.HashMap
import org.jgrapht.alg.DijkstraShortestPath
import org.sireum.jawa.alir.JawaAlirInfoProvider
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.Center
import org.sireum.jawa.alir.interProcedural.Callee
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.JawaCodeSource
import java.util.regex.Pattern
import org.sireum.jawa.alir.callGraph.CallGraph
import org.jgrapht.ext.EdgeNameProvider

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class InterproceduralControlFlowGraph[Node <: ICFGNode] extends InterProceduralGraph[Node]{
  private var succBranchMap : MMap[(Node, Option[Branch]), Node] = null
  private var predBranchMap : MMap[(Node, Option[Branch]), Node] = null
  val BRANCH_PROPERTY_KEY = ControlFlowGraph.BRANCH_PROPERTY_KEY
  final val EDGE_TYPE = "EdgeType"
  
  def addEdge(source : Node, target : Node, typ : String) : Edge = {
    val e = addEdge(source, target)
    if(typ != null)
    	e.setProperty(EDGE_TYPE, typ)
    e
  }
  
  def isEdgeType(e : Edge, typ : String) : Boolean = {
    e.getPropertyOrElse[String](EDGE_TYPE, null) == typ
  }
    
  protected var entryN : ICFGNode = null

  protected var exitN : ICFGNode = null
  
  val centerContext = new Context(GlobalConfig.ICFG_CONTEXT_K)
  centerContext.setContext("Center", "L0000")
  private val cNode = addICFGCenterNode(centerContext)
  cNode.setOwner(Center.CENTER_PROCEDURE_SIG)
  protected var centerN : ICFGNode = cNode
  
  def centerNode : Node = this.centerN.asInstanceOf[Node]
  
  def entryNode : Node = this.entryN.asInstanceOf[Node]
  
  def exitNode : Node = this.exitN.asInstanceOf[Node]
  
  private val cg : CallGraph = new CallGraph
  
  def getCallGraph : CallGraph = this.cg
  
  private val processed : MMap[(String, Context), ISet[Node]] = new HashMap[(String, Context), ISet[Node]] with SynchronizedMap[(String, Context), ISet[Node]]
  
  def isProcessed(proc : String, callerContext : Context) : Boolean = processed.contains(proc, callerContext)
  
  def addProcessed(jp : String, c : Context, nodes : ISet[Node]) = {
    this.processed += ((jp, c) -> nodes)
  }
  
  def getProcessed = this.processed
  
  def entryNode(proc : String, callerContext : Context) : Node = {
    require(isProcessed(proc, callerContext))
    processed(proc, callerContext).foreach{
      n => if(n.isInstanceOf[ICFGEntryNode]) return n
    }
    throw new RuntimeException("Cannot find entry node for: " + proc)
  }
  
  def reverse : InterproceduralControlFlowGraph[Node] = {
    val result = new InterproceduralControlFlowGraph[Node]
    for (n <- nodes) result.addNode(n)
    for (e <- edges) result.addEdge(e.target, e.source)
    result.entryN = this.exitNode
    result.exitN = this.entryNode
    result.centerN = this.centerN
    result
  }
  
//  def merge(cg : CallGraph[Node]) = {
//    this.pl ++= cg.pool
//    cg.nodes.foreach(n => addNode(n))
//    cg.edges.foreach(e => addEdge(e))
//    this.processed ++= cg.getProcessed
//  }
    
  private def putBranchOnEdge(trans : Int, branch : Int, e : Edge) = {
    e(BRANCH_PROPERTY_KEY) = (trans, branch)
  }

  private def getBranch(pst : ProcedureSymbolTable, e : Edge) : Option[Branch] = {
    if (e ? BRANCH_PROPERTY_KEY) {
      val p : (Int, Int) = e(BRANCH_PROPERTY_KEY)
      var j =
        PilarAstUtil.getJumps(pst.location(
          e.source.asInstanceOf[AlirLocationNode].locIndex))(first2(p)).get
      val i = second2(p)

      if (j.isInstanceOf[CallJump])
        j = j.asInstanceOf[CallJump].jump.get

      (j : @unchecked) match {
        case gj : GotoJump   => Some(gj)
        case rj : ReturnJump => Some(rj)
        case ifj : IfJump =>
          if (i == 0) ifj.ifElse
          else Some(ifj.ifThens(i - 1))
        case sj : SwitchJump =>
          if (i == 0) sj.defaultCase
          else (Some(sj.cases(i - 1)))
      }
    } else None
  }

	def useBranch[T](pst : ProcedureSymbolTable)(f : => T) : T = {
	  succBranchMap = mmapEmpty
	  predBranchMap = mmapEmpty
	  for (node <- this.nodes) {
	    for (succEdge <- successorEdges(node)) {
	      val b = getBranch(pst, succEdge)
	      val s = edgeSource(succEdge)
	      val t = edgeTarget(succEdge)
	      succBranchMap((node, b)) = t
	      predBranchMap((t, b)) = s
	    }
	  }
	  val result = f
	  succBranchMap = null
	  predBranchMap = null
	  result
	}
    
    
    def successor(node : Node, branch : Option[Branch]) : Node = {
      assert(succBranchMap != null,
        "The successor method needs useBranch as enclosing context")
      succBranchMap((node, branch))
    }

    def predecessor(node : Node, branch : Option[Branch]) : Node = {
      assert(predBranchMap != null,
        "The successor method needs useBranch as enclosing context")
      predBranchMap((node, branch))
    }

    override def toString = {
      val sb = new StringBuilder("system CFG\n")

      for (n <- nodes)
        for (m <- successors(n)) {
          for (e <- getEdges(n, m)) {
            val branch = if (e ? BRANCH_PROPERTY_KEY)
              e(BRANCH_PROPERTY_KEY).toString
            else ""
              sb.append("%s -> %s %s\n".format(n, m, branch))
          }
        }

      sb.append("\n")

      sb.toString
    }
  
  /**
   * (We ASSUME that predecessors ???? and successors of n are within the same procedure as of n)
   * So, this algorithm is only for an internal node of a procedure NOT for a procedure's Entry node or Exit node
   * The algorithm is obvious from the following code 
   */
  def compressByDelNode (n : Node) = {
    val preds = predecessors(n) - n
    val succs = successors(n) - n
    deleteNode(n)
    for(pred <- preds){
      for(succ <- succs){           
        if (!hasEdge(pred,succ)){
          addEdge(pred, succ)
        }
      }
    }
  }
  
  val simpleCallGraphVlabelProvider = new VertexNameProvider[Node]() {
    def filterLabel(uri : String) = {
      uri.filter(_.isUnicodeIdentifierPart)  // filters out the special characters like '/', '.', '%', etc.  
    }
      
    def getVertexName(v : Node) : String = {
      if(!v.isInstanceOf[ICFGCallNode]) throw new RuntimeException("Simple call graph should only contain call node!")
      filterLabel(v.asInstanceOf[ICFGCallNode].getOwner)
    }
  }
  
  val simpleCallGraphElabelProvider = new EdgeNameProvider[Edge]() {
    def filterLabel(uri : String) = {
      uri.filter(_.isUnicodeIdentifierPart)  // filters out the special characters like '/', '.', '%', etc.  
    }
    
    def getEdgeName(e : Edge) : String = {
      if(!e.source.isInstanceOf[ICFGCallNode] || !e.target.isInstanceOf[ICFGCallNode]) throw new RuntimeException("Simple call graph should only contain call node!")
      filterLabel(e.source.asInstanceOf[ICFGCallNode].getOwner + "-calls->" + e.target.asInstanceOf[ICFGCallNode].getOwner)
    }
  }
  
  val detailedCallGraphVlabelProvider = new VertexNameProvider[Node]() {
    def filterLabel(uri : String) = {
      uri.filter(_.isUnicodeIdentifierPart)  // filters out the special characters like '/', '.', '%', etc.  
    }
      
    def getVertexName(v : Node) : String = {
      throw new RuntimeException("Not implemented yet!")
    }
  }
  
  // read the sCfg and build a corresponding DFA/NFA  
  def buildAutomata() : Automaton = {
    val automata = new Automaton()
    // build a map between sCfg-nodes-set and automata-nodes-set
    val nodeMap:MMap[Node, State] = mmapEmpty
    
    nodes.foreach(
        gNode => {
          val state = new State()
          state.setAccept(true)  // making each state in the automata an accept state
          nodeMap(gNode) = state
        }
    )
    // build a map between Entry-nodes-set (in sCfg) to English characters (assuming Entry-nodes-set is small); note that each call corresponds to an edge to Entry node of callee proc
    val calleeMap:MMap[Node, Char] = mmapEmpty
    var calleeIndex = 0
    nodes.foreach(
        gNode => {   // ******* below check the hard-coded path for testing ***********
          if(!gNode.toString().contains("pilar:/procedure/default/%5B%7Cde::mobinauten") && gNode.toString().endsWith(".Entry"))
          {
            calleeMap(gNode) = ('A' + calleeIndex).toChar
            println("in calleeMap: node " + gNode.toString() + "  has label = " + calleeMap(gNode))
            calleeIndex = calleeIndex + 1
          }
        }
    )
    // build the automata from the sCfg
    
    nodes.foreach(
        gNode => {
          val automataNode = nodeMap(gNode)   // automataNode = automata state
          val gSuccs = successors(gNode)
          var label: Char = 'x'  // default label of automata transition
          gSuccs.foreach(
              gSucc => {
                val automataSucc = nodeMap(gSucc)
                if(calleeMap.contains(gSucc))
                  label = calleeMap(gSucc)
                val tr = new Transition(label, automataSucc)
                automataNode.addTransition(tr)
              }
          )
        }
    )
   automata
  }
   
  def isCall(l : LocationDecl) : Boolean = l.isInstanceOf[JumpLocation] && l.asInstanceOf[JumpLocation].jump.isInstanceOf[CallJump]
   
  def merge(icfg : InterproceduralControlFlowGraph[Node]) = {
    this.pl ++= icfg.pool
    icfg.nodes.foreach(addNode(_))
    icfg.edges.foreach(addEdge(_))
    icfg.getCallGraph.getCallMap.foreach{
      case (src, dsts) =>
        cg.addCalls(src, cg.getCallMap.getOrElse(src, isetEmpty) ++ dsts)
    }
    this.processed ++= icfg.processed
    this.predBranchMap ++= icfg.predBranchMap
    this.succBranchMap ++= icfg.succBranchMap
  }
  
  def collectCfgToBaseGraph[VirtualLabel](calleeProc : JawaProcedure, callerContext : Context, isFirst : Boolean = false) = {
    this.synchronized{
	    val calleeSig = calleeProc.getSignature
	    if(!calleeProc.checkLevel(Center.ResolveLevel.BODY)) calleeProc.resolveBody
	    val body = calleeProc.getProcedureBody
	    val rawcode = JawaCodeSource.getProcedureCodeWithoutFailing(calleeProc.getSignature)
	    val codes = rawcode.split("\\r?\\n")
	    val cfg = JawaAlirInfoProvider.getCfg(calleeProc)
	    var nodes = isetEmpty[Node]
	    cfg.nodes map{
	      n =>
		      n match{
		        case vn : AlirVirtualNode[VirtualLabel] =>
		          vn.label.toString match{
		            case "Entry" => 
		              val entryNode = addICFGEntryNode(callerContext.copy.setContext(calleeSig, "Entry"))
		              entryNode.setOwner(calleeProc.getSignature)
		              nodes += entryNode
		              if(isFirst) this.entryN = entryNode
		            case "Exit" => 
		              val exitNode = addICFGExitNode(callerContext.copy.setContext(calleeSig,  "Exit"))
		              exitNode.setOwner(calleeProc.getSignature)
		              nodes += exitNode
		              if(isFirst) this.exitN = exitNode
		            case a => throw new RuntimeException("unexpected virtual label: " + a)
		          }
		        case ln : AlirLocationUriNode=>
		          val l = body.location(ln.locIndex)
		          val code = codes.find(_.contains("#" + ln.locUri + ".")).getOrElse(throw new RuntimeException("Could not find " + ln.locUri + " from \n" + rawcode))
		          if(isCall(l)){
	              val c = addICFGCallNode(callerContext.copy.setContext(calleeSig, ln.locUri))
	              c.setOwner(calleeProc.getSignature)
	              c.setCode(code)
	              c.asInstanceOf[ICFGLocNode].setLocIndex(ln.locIndex)
	              nodes += c
	              val r = addICFGReturnNode(callerContext.copy.setContext(calleeSig, ln.locUri))
	              r.setOwner(calleeProc.getSignature)
	              r.setCode(code)
	              r.asInstanceOf[ICFGLocNode].setLocIndex(ln.locIndex)
	              nodes += r
	//              addEdge(c, r)
		          } else {
		            val node = addICFGNormalNode(callerContext.copy.setContext(calleeSig, ln.locUri))
		            node.setOwner(calleeProc.getSignature)
		            node.setCode(code)
		            node.asInstanceOf[ICFGLocNode].setLocIndex(ln.locIndex)
		            nodes += node
		          }
		        case a : AlirLocationNode => 
		          // should not have a chance to reach here.
		          val node = addICFGNormalNode(callerContext.copy.setContext(calleeSig, a.locIndex.toString))
		          node.setOwner(calleeProc.getSignature)
		          node.setCode("unknown")
		          node.asInstanceOf[ICFGLocNode].setLocIndex(a.locIndex)
		          nodes += node
		      }
	    }
	    for (e <- cfg.edges) {
	      val entryNode = getICFGEntryNode(callerContext.copy.setContext(calleeSig, "Entry"))
	      val exitNode = getICFGExitNode(callerContext.copy.setContext(calleeSig, "Exit"))
	      e.source match{
	        case vns : AlirVirtualNode[VirtualLabel] =>
	          e.target match{
	            case vnt : AlirVirtualNode[VirtualLabel] =>
	              addEdge(entryNode, exitNode)
	            case lnt : AlirLocationUriNode =>
	              val lt = body.location(lnt.locIndex)
			          if(isCall(lt)){
	                val callNodeTarget = getICFGCallNode(callerContext.copy.setContext(calleeSig, lnt.locUri))
	                addEdge(entryNode, callNodeTarget)
			          } else {
		              val targetNode = getICFGNormalNode(callerContext.copy.setContext(calleeSig, lnt.locUri))
		              addEdge(entryNode, targetNode)
			          }
	            case nt =>
	              val targetNode = getICFGNormalNode(callerContext.copy.setContext(calleeSig, nt.toString))
	              addEdge(entryNode, targetNode)
	          }
	        case lns : AlirLocationUriNode =>
	          val ls = body.location(lns.locIndex)
	          e.target match{
	            case vnt : AlirVirtualNode[VirtualLabel] =>
	              if(isCall(ls)){
	                val returnNodeSource = getICFGReturnNode(callerContext.copy.setContext(calleeSig, lns.locUri))
	                addEdge(returnNodeSource, exitNode)
	              } else {
	                val sourceNode = getICFGNormalNode(callerContext.copy.setContext(calleeSig, lns.locUri))
	              	addEdge(sourceNode, exitNode)
	              }
	            case lnt : AlirLocationUriNode =>
	              val lt = body.location(lnt.locIndex)
	              if(isCall(ls)){
	                val returnNodeSource = getICFGReturnNode(callerContext.copy.setContext(calleeSig, lns.locUri))
	                if(isCall(lt)){
		                val callNodeTarget = getICFGCallNode(callerContext.copy.setContext(calleeSig, lnt.locUri))
		                addEdge(returnNodeSource, callNodeTarget)
				          } else {
			              val targetNode = getICFGNormalNode(callerContext.copy.setContext(calleeSig, lnt.locUri))
			              addEdge(returnNodeSource, targetNode)
				          }
	              } else {
	                val sourceNode = getICFGNormalNode(callerContext.copy.setContext(calleeSig, lns.locUri))
	                if(isCall(lt)){
		                val callNodeTarget = getICFGCallNode(callerContext.copy.setContext(calleeSig, lnt.locUri))
		                addEdge(sourceNode, callNodeTarget)
				          } else {
			              val targetNode = getICFGNormalNode(callerContext.copy.setContext(calleeSig, lnt.locUri))
			              addEdge(sourceNode, targetNode)
				          }
	              }
	            case nt =>
	              val targetNode = getICFGNormalNode(callerContext.copy.setContext(calleeSig, nt.toString))
	              if(isCall(ls)){
	                val returnNodeSource = getICFGReturnNode(callerContext.copy.setContext(calleeSig, lns.locUri))
	                addEdge(returnNodeSource, targetNode)
	              } else {
	                val sourceNode = getICFGNormalNode(callerContext.copy.setContext(calleeSig, lns.locUri))
	                addEdge(sourceNode, targetNode)
	              }
	          }
	        case ns =>
	          val sourceNode = getICFGNormalNode(callerContext.copy.setContext(calleeSig, ns.toString))
	          e.target match{
	            case vnt : AlirVirtualNode[VirtualLabel] =>
	              addEdge(sourceNode, exitNode)
	            case lnt : AlirLocationUriNode =>
	              val lt = body.location(lnt.locIndex)
			          if(isCall(lt)){
	                val callNodeTarget = getICFGCallNode(callerContext.copy.setContext(calleeSig, lnt.locUri))
	                val returnNodeTarget = getICFGReturnNode(callerContext.copy.setContext(calleeSig, lnt.locUri))
	                addEdge(sourceNode, callNodeTarget)
			          } else {
		              val targetNode = getICFGNormalNode(callerContext.copy.setContext(calleeSig, lnt.locUri))
		              addEdge(sourceNode, targetNode)
			          }
	            case nt =>
	              val targetNode = getICFGNormalNode(callerContext.copy.setContext(calleeSig, nt.toString))
	              addEdge(sourceNode, targetNode)
	          }
	      }
	    }
	    addProcessed(calleeProc.getSignature, callerContext, nodes)
	    nodes
    }
  }
  
  def extendGraph(calleeSig  : String, callerContext : Context) : Node = {
    val callNode = getICFGCallNode(callerContext)
    val returnNode = getICFGReturnNode(callerContext)
    val calleeEntryContext = callerContext.copy
    calleeEntryContext.setContext(calleeSig, "Entry")
    val calleeExitContext = callerContext.copy
    calleeExitContext.setContext(calleeSig, "Exit")
    val targetNode = getICFGEntryNode(calleeEntryContext)
    val retSrcNode = getICFGExitNode(calleeExitContext)
    this.synchronized{
      this.cg.addCall(callNode.getOwner, targetNode.getOwner)
      if(!hasEdge(callNode, targetNode))
      	addEdge(callNode, targetNode)
      if(!hasEdge(retSrcNode, returnNode))
      	addEdge(retSrcNode, returnNode)
    }
    targetNode
  }
  
  def extendGraphOneWay(calleeSig  : String, callerContext : Context, typ : String = null) : Node = {
    val callNode = getICFGCallNode(callerContext)
    val calleeEntryContext = callerContext.copy
    calleeEntryContext.setContext(calleeSig, "Entry")
    val targetNode = getICFGEntryNode(calleeEntryContext)
    this.synchronized{
      this.cg.addCall(callNode.getOwner, targetNode.getOwner)
      if(!hasEdge(callNode, targetNode))
        addEdge(callNode, targetNode, typ)
    }
    targetNode
  }
  
  def toSimpleCallGraph : InterproceduralControlFlowGraph[Node] = {
    val ns = nodes filter{
      n =>
        n match{
          case cn : ICFGCallNode =>
            false
          case _ => true
        }
    }
    ns foreach(compressByDelNode(_))
    this
  }
  
  def toDetailedCallGraph : InterproceduralControlFlowGraph[Node] = {
    throw new RuntimeException("Have not implemented yet.")
  }
  
  def toApiGraph : InterproceduralControlFlowGraph[Node] = {
    val ns = nodes filter{
      n =>
        n match{
          case cn : ICFGCallNode =>
            cn.getCalleeSet.exists { c => !c.callee.getDeclaringRecord.isFrameworkRecord }
          case _ => true
        }
    }
    ns foreach(compressByDelNode(_))
    this
  }
  
  private def getSignatureFromCallNode(node : Node) : String = {
    assume(node.isInstanceOf[ICFGCallNode])
    val loc = Center.getProcedureWithoutFailing(node.getOwner).getProcedureBody.location(node.asInstanceOf[ICFGCallNode].getLocIndex)
    val sig = loc.asInstanceOf[JumpLocation].jump.getValueAnnotation("signature") match {
      case Some(s) => s match {
        case ne : NameExp => ne.name.name
        case _ => ""
      }
      case None => throw new RuntimeException("cannot found annotation 'signature' from: " + loc)
    }
    sig
  }
  
  def toText(w : Writer)  = {
    var res : String = ""
    res += "Nodes:\n"
    nodes.foreach{
      node =>
        val nStr = node.getContext.toFullString + " ::::> " + node.getCode
        res += nStr + "\n"
    }
    res += "Edges:\n"
    edges.foreach{
      edge =>
        val eStr = edge.source.getContext.toFullString + " --> " + edge.target.getContext.toFullString
        res += eStr + "\n"
    }
    w.write(res)
  }
  
  def addICFGNormalNode(context : Context) : Node = {
    val node = newICFGNormalNode(context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def icfgNormalNodeExists(context : Context) : Boolean = {
    graph.containsVertex(newICFGNormalNode(context).asInstanceOf[Node])
  }
  
  def getICFGNormalNode(context : Context) : Node =
    pool(newICFGNormalNode(context))
  
  protected def newICFGNormalNode(context : Context) =
    ICFGNormalNode(context)
    
  def addICFGCallNode(context : Context) : Node = {
    val node = newICFGCallNode(context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def icfgCallNodeExists(context : Context) : Boolean = {
    graph.containsVertex(newICFGCallNode(context).asInstanceOf[Node])
  }
  
  def getICFGCallNode(context : Context) : Node =
    pool(newICFGCallNode(context))
  
  protected def newICFGCallNode(context : Context) =
    ICFGCallNode(context)
    
  def addICFGReturnNode(context : Context) : Node = {
    val node = newICFGReturnNode(context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def icfgReturnNodeExists(context : Context) : Boolean = {
    graph.containsVertex(newICFGReturnNode(context).asInstanceOf[Node])
  }
  
  def getICFGReturnNode(context : Context) : Node =
    pool(newICFGReturnNode(context))
  
  protected def newICFGReturnNode(context : Context) =
    ICFGReturnNode(context)
  
    
  def addICFGEntryNode(context : Context) : Node = {
    val node = newICFGEntryNode(context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def icfgEntryNodeExists(context : Context) : Boolean = {
    graph.containsVertex(newICFGEntryNode(context).asInstanceOf[Node])
  }
  
  def getICFGEntryNode(context : Context) : Node =
    pool(newICFGEntryNode(context))
  
  protected def newICFGEntryNode(context : Context) =
    ICFGEntryNode(context)
    
  def addICFGCenterNode(context : Context) : Node = {
    val node = newICFGCenterNode(context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def icfgICFGCenterNodeExists(context : Context) : Boolean = {
    graph.containsVertex(newICFGCenterNode(context).asInstanceOf[Node])
  }
  
  def getICFGCenterNode(context : Context) : Node =
    pool(newICFGCenterNode(context))
  
  protected def newICFGCenterNode(context : Context) =
    ICFGCenterNode(context)
    
  def addICFGExitNode(context : Context) : Node = {
    val node = newICFGExitNode(context).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def icfgExitNodeExists(context : Context) : Boolean = {
    graph.containsVertex(newICFGExitNode(context).asInstanceOf[Node])
  }
  
  def getICFGExitNode(context : Context) : Node =
    pool(newICFGExitNode(context))
  
  protected def newICFGExitNode(context : Context) =
    ICFGExitNode(context)
  
}

sealed abstract class ICFGNode(context : Context) extends InterProceduralNode(context){
  protected var owner : String = null
  protected var loadedClassBitSet : BitSet = BitSet.empty
  protected var code : String = null
  def setOwner(owner : String)  = this.owner = owner
  def getOwner = this.owner
  def setCode(code : String) = this.code = code
  def getCode : String = this.code
  def setLoadedClassBitSet(bitset : BitSet) = this.loadedClassBitSet = bitset
  def getLoadedClassBitSet = this.loadedClassBitSet
  
//  def updateLoadedClassBitSet(bitset : BitSet) = {
//    if(getLoadedClassBitSet == BitSet.empty) setLoadedClassBitSet(bitset)
//    else setLoadedClassBitSet(bitset.intersect(getLoadedClassBitSet))
//  }
}

abstract class ICFGVirtualNode(context : Context) extends ICFGNode(context) {
  def getVirtualLabel : String
  
  override def toString : String = getVirtualLabel + "@" + context
}

final case class ICFGEntryNode(context : Context) extends ICFGVirtualNode(context){
  this.code = "Entry: " + context.getProcedureSig
  def getVirtualLabel : String = "Entry"
}

final case class ICFGExitNode(context : Context) extends ICFGVirtualNode(context){
  this.code = "Exit: " + context.getProcedureSig
  def getVirtualLabel : String = "Exit"
}

final case class ICFGCenterNode(context : Context) extends ICFGVirtualNode(context){
  this.code = "L0000: Center;"
  def getVirtualLabel : String = "Center"
}

abstract class ICFGLocNode(context : Context) extends ICFGNode(context) {
  def getLocUri : String = context.getLocUri
  protected val LOC_INDEX = "LocIndex"
  def setLocIndex(i : Int) = setProperty(LOC_INDEX, i)
  def getLocIndex : Int = getPropertyOrElse[Int](LOC_INDEX, throw new RuntimeException("did not have loc index"))
}

abstract class ICFGInvokeNode(context : Context) extends ICFGLocNode(context) {
  final val CALLEES = "callee_set"
  def getInvokeLabel : String
  def setCalleeSet(calleeSet : ISet[Callee]) = this.setProperty(CALLEES, calleeSet)
  def getCalleeSet : ISet[Callee] = this.getPropertyOrElse(CALLEES, isetEmpty)
  override def toString : String = getInvokeLabel + "@" + context
}

final case class ICFGCallNode(context : Context) extends ICFGInvokeNode(context){
  def getInvokeLabel : String = "Call"
}

final case class ICFGReturnNode(context : Context) extends ICFGInvokeNode(context){
  def getInvokeLabel : String = "Return"
}

final case class ICFGNormalNode(context : Context) extends ICFGLocNode(context){
  override def toString : String = context.toString
}
