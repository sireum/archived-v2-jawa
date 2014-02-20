package org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis

import org.sireum.jawa._
import org.sireum.jawa.alir.interProcedural.controlFlowGraph._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.pilar.ast._
import org.sireum.util._
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.RFAFact
import org.sireum.jawa.alir.interProcedural.InstanceCallee
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.VarSlot
import org.sireum.jawa.alir.interProcedural.InterProceduralNode
import org.sireum.jawa.alir.interProcedural.InterProceduralGraph

class InterProceduralDataDependenceGraph[Node <: IDDGNode] extends InterProceduralGraph[Node]{
	
  protected var centerN : IDDGCenterNode = null
  def centerNode : Node = this.centerN.asInstanceOf[Node]
  
  protected var entryN : IDDGEntryNode = null
  def entryNode : Node = this.entryN.asInstanceOf[Node]
  
  protected var cg : InterproceduralControlFlowGraph[CGNode] = null
  
	def initGraph(cg : InterproceduralControlFlowGraph[CGNode]) = {
    this.cg = cg
	  cg.nodes.foreach{
	    node =>
	      node match{
	        case en : CGEntryNode =>
	          val owner = en.getOwner
	          val pnames = owner.getParamNames
	          val ptyps = owner.getParamTypes
	          var position = 0
	          for(i <- 0 to pnames.size - 1){
	            val ptypName = ptyps(i).name
	            val n = addIDDGEntryParamNode(en, position)
	            n.asInstanceOf[IDDGEntryParamNode].paramName = pnames(i)
	            if(ptypName == "[|double|]" || ptypName == "[|long|]"){
	              position += 1
	              val n = addIDDGEntryParamNode(en, position)
	              n.asInstanceOf[IDDGEntryParamNode].paramName = pnames(i)
	            }
	            position += 1
	          }
	        case en : CGExitNode =>
	          val owner = en.getOwner
	          val pnames = owner.getParamNames
	          for(i <- 0 to pnames.size - 1){
	            val n = addIDDGExitParamNode(en, i)
	            n.asInstanceOf[IDDGExitParamNode].paramName = pnames(i)
	          }
	        case en : CGCenterNode =>
	        case cn : CGCallNode =>
	          val loc = cn.getOwner.getProcedureBody.location(cn.getLocIndex)
	          val argNames : MList[String] = mlistEmpty
	          loc match{
	            case jumploc : JumpLocation =>
	              jumploc.jump match {
	                case t : CallJump if t.jump.isEmpty =>
	                  t.callExp.arg match {
						          case te : TupleExp =>
						            val exps = te.exps
						            for(i <- 0 to (exps.size-1)) {
						              val varName = exps(i) match{
						                case ne : NameExp => ne.name.name
						                case a => a.toString()
						              }
						              argNames += varName
						            	val n = addIDDGCallArgNode(cn, i)
						            	n.asInstanceOf[IDDGCallArgNode].argName = varName
				                }
						          case _ =>
						        }
	                case _ =>
	              }
	            case _ =>
	          }
	          val rn = addIDDGReturnVarNode(cn)
	          if(cn.getCalleeSet.exists(p => p.calleeProc.getDeclaringRecord.isLibraryRecord)){
	            val vn = addIDDGVirtualBodyNode(cn)
	            vn.asInstanceOf[IDDGVirtualBodyNode].argNames = argNames.toList
	          }
	        case rn : CGReturnNode =>
	          val loc = rn.getOwner.getProcedureBody.location(rn.getLocIndex)
	          loc match{
	            case jumploc : JumpLocation =>
	              jumploc.jump match {
	                case t : CallJump if t.jump.isEmpty =>
	                  t.callExp.arg match {
						          case te : TupleExp =>
						            val exps = te.exps
						            for(i <- 0 to (exps.size-1)) {
						              val varName = exps(i) match{
						                case ne : NameExp => ne.name.name
						                case a => a.toString()
						              }
						            	val n = addIDDGReturnArgNode(rn, i)
						            	n.asInstanceOf[IDDGReturnArgNode].argName = varName
				                }
						          case _ =>
						        }
	                case _ =>
	              }
	            case _ =>
	          }
	        case nn : CGNormalNode => addIDDGNormalNode(nn)
	        case _ =>
	      }
	  }
    this.centerN = addIDDGCenterNode(cg.centerNode.asInstanceOf[CGCenterNode]).asInstanceOf[IDDGCenterNode]
    this.entryN = addIDDGEntryNode(cg.entryNode.asInstanceOf[CGEntryNode]).asInstanceOf[IDDGEntryNode]
	}
	
	def findDefSite(defSite : Context) : Node = {
	  val cgN = {
	    if(this.cg.cgNormalNodeExists(defSite)) this.cg.getCGNormalNode(defSite)
		  else if(this.cg.cgCallNodeExists(defSite)) this.cg.getCGCallNode(defSite)
		  else if(defSite.toString == "(EntryPoint,L0000)") this.cg.entryNode
		  else if(defSite.toString == "(Center,L0000)") this.cg.centerNode
		  else throw new RuntimeException("Cannot find node: " + defSite)
	  }
	  if(cgN.isInstanceOf[CGNormalNode] && iddgNormalNodeExists(cgN.asInstanceOf[CGNormalNode])) getIDDGNormalNode(cgN.asInstanceOf[CGNormalNode])
	  else if(cgN.isInstanceOf[CGCallNode] && iddgVirtualBodyNodeExists(cgN.asInstanceOf[CGCallNode])) getIDDGVirtualBodyNode(cgN.asInstanceOf[CGCallNode])
	  else if(cgN.isInstanceOf[CGCallNode] && iddgReturnVarNodeExists(cgN.asInstanceOf[CGCallNode])) getIDDGReturnVarNode(cgN.asInstanceOf[CGCallNode])
	  else if(cgN.isInstanceOf[CGEntryNode]) this.entryNode
	  else if(cgN.isInstanceOf[CGCenterNode]) this.centerNode
	  else throw new RuntimeException("Cannot find node: " + defSite)
	}
	
	def findDefSite(defSite : Context, position : Int) : Node = {
	  val cgN = {
	    if(this.cg.cgCallNodeExists(defSite)) this.cg.getCGCallNode(defSite)
	    else if(this.cg.cgReturnNodeExists(defSite)) this.cg.getCGReturnNode(defSite)
	    else if(this.cg.cgEntryNodeExists(defSite)) this.cg.getCGEntryNode(defSite)
		  else throw new RuntimeException("Cannot find node: " + defSite)
	  }
	  if(cgN.isInstanceOf[CGCallNode] && iddgCallArgNodeExists(cgN.asInstanceOf[CGCallNode], position)) getIDDGCallArgNode(cgN.asInstanceOf[CGCallNode], position)
	  else if(cgN.isInstanceOf[CGReturnNode] && iddgReturnArgNodeExists(cgN.asInstanceOf[CGReturnNode], position)) getIDDGReturnArgNode(cgN.asInstanceOf[CGReturnNode], position)
	  else if(cgN.isInstanceOf[CGEntryNode] && iddgEntryParamNodeExists(cgN.asInstanceOf[CGEntryNode], position)) getIDDGEntryParamNode(cgN.asInstanceOf[CGEntryNode], position)
	  else throw new RuntimeException("Cannot find node: " + defSite + ":" + position)
	}
	
	def extendGraphForSinkApis(callArgNode : IDDGCallArgNode, rfaFacts : ISet[RFAFact]) = {
	  val calleeSet = callArgNode.getCalleeSet
    calleeSet.foreach{
      callee =>
        val calleep = callee.calleeProc
        val argSlot = VarSlot(callArgNode.argName)
        val argFacts = rfaFacts.filter(fact=> argSlot == fact.s)
			  val argRelatedFacts = ReachingFactsAnalysisHelper.getRelatedHeapFactsFrom(argFacts, rfaFacts)
		    argRelatedFacts.foreach{
          case RFAFact(slot, ins) =>
		      	val t = findDefSite(ins.getDefSite)
		      	addEdge(callArgNode.asInstanceOf[Node], t)
        }
        argFacts.foreach{
          case RFAFact(slot, argIns) => 
	          argIns.getFieldsUnknownDefSites.foreach{
	          	case (defsite, udfields) =>
	          	  if(callArgNode.getContext != defsite){
		          	  val t = findDefSite(defsite)
		          	  addEdge(callArgNode.asInstanceOf[Node], t)
	          	  }
	        	}
        }
	  }
	}
  
  def iddgEntryParamNodeExists(cgN : CGEntryNode, position : Int) : Boolean = {
    graph.containsVertex(newIDDGEntryParamNode(cgN, position).asInstanceOf[Node])
  }

  def addIDDGEntryParamNode(cgN : CGEntryNode, position : Int) : Node = {
    val node = newIDDGEntryParamNode(cgN, position).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGEntryParamNode(cgN : CGEntryNode, position : Int) : Node =
    pool(newIDDGEntryParamNode(cgN, position))
    
  protected def newIDDGEntryParamNode(cgN : CGEntryNode, position : Int) =
    IDDGEntryParamNode(cgN, position)
	
  def iddgExitParamNodeExists(cgN : CGExitNode, position : Int) : Boolean = {
    graph.containsVertex(newIDDGExitParamNode(cgN, position).asInstanceOf[Node])
  }

  def addIDDGExitParamNode(cgN : CGExitNode, position : Int) : Node = {
    val node = newIDDGExitParamNode(cgN, position).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGExitParamNode(cgN : CGExitNode, position : Int) : Node =
    pool(newIDDGExitParamNode(cgN, position))
    
  protected def newIDDGExitParamNode(cgN : CGExitNode, position : Int) =
    IDDGExitParamNode(cgN, position)
    
  def iddgCallArgNodeExists(cgN : CGCallNode, position : Int) : Boolean = {
    graph.containsVertex(newIDDGCallArgNode(cgN, position).asInstanceOf[Node])
  }

  def addIDDGCallArgNode(cgN : CGCallNode, position : Int) : Node = {
    val node = newIDDGCallArgNode(cgN, position).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGCallArgNode(cgN : CGCallNode, position : Int) : Node =
    pool(newIDDGCallArgNode(cgN, position))
    
  def getIDDGCallArgNodes(cgN : CGCallNode) : Set[Node] = {
    val result : MSet[Node] = msetEmpty
    var position = 0
    while(iddgCallArgNodeExists(cgN, position)){
    	result += pool(newIDDGCallArgNode(cgN, position))
    	position += 1
    }
    result.toSet
  }
    
  protected def newIDDGCallArgNode(cgN : CGCallNode, position : Int) = IDDGCallArgNode(cgN, position)
    
  def iddgReturnArgNodeExists(cgN : CGReturnNode, position : Int) : Boolean = {
    graph.containsVertex(newIDDGReturnArgNode(cgN, position).asInstanceOf[Node])
  }

  def addIDDGReturnArgNode(cgN : CGReturnNode, position : Int) : Node = {
    val node = newIDDGReturnArgNode(cgN, position).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGReturnArgNode(cgN : CGReturnNode, position : Int) : Node =
    pool(newIDDGReturnArgNode(cgN, position))
    
  protected def newIDDGReturnArgNode(cgN : CGReturnNode, position : Int) = IDDGReturnArgNode(cgN, position)
    
  def iddgReturnVarNodeExists(cgN : CGCallNode) : Boolean = {
    graph.containsVertex(newIDDGReturnVarNode(cgN).asInstanceOf[Node])
  }

  def addIDDGReturnVarNode(cgN : CGCallNode) : Node = {
    val node = newIDDGReturnVarNode(cgN).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGReturnVarNode(cgN : CGCallNode) : Node =
    pool(newIDDGReturnVarNode(cgN))
    
  protected def newIDDGReturnVarNode(cgN : CGCallNode) =
    IDDGReturnVarNode(cgN)
    
  def iddgVirtualBodyNodeExists(cgN : CGCallNode) : Boolean = {
    graph.containsVertex(newIDDGVirtualBodyNode(cgN).asInstanceOf[Node])
  }
  
  def addIDDGVirtualBodyNode(cgN : CGCallNode) : Node = {
    val node = newIDDGVirtualBodyNode(cgN).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGVirtualBodyNode(cgN : CGCallNode) : Node =
    pool(newIDDGVirtualBodyNode(cgN))
    
  protected def newIDDGVirtualBodyNode(cgN : CGCallNode) =
    IDDGVirtualBodyNode(cgN)
    
  def iddgNormalNodeExists(cgN : CGNormalNode) : Boolean = {
    graph.containsVertex(newIDDGNormalNode(cgN).asInstanceOf[Node])
  }
  
  def addIDDGNormalNode(cgN : CGNormalNode) : Node = {
    val node = newIDDGNormalNode(cgN).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGNormalNode(cgN : CGNormalNode) : Node =
    pool(newIDDGNormalNode(cgN))
    
  protected def newIDDGNormalNode(cgN : CGNormalNode) =
    IDDGNormalNode(cgN)
    
  def iddgCenterNodeExists(cgN : CGCenterNode) : Boolean = {
    graph.containsVertex(newIDDGCenterNode(cgN).asInstanceOf[Node])
  }
  
  def addIDDGCenterNode(cgN : CGCenterNode) : Node = {
    val node = newIDDGCenterNode(cgN).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGCenterNode(cgN : CGCenterNode) : Node =
    pool(newIDDGCenterNode(cgN))
    
  protected def newIDDGCenterNode(cgN : CGCenterNode) =
    IDDGCenterNode(cgN)
    
  def iddgEntryNodeExists(cgN : CGEntryNode) : Boolean = {
    graph.containsVertex(newIDDGEntryNode(cgN).asInstanceOf[Node])
  }
  
  def addIDDGEntryNode(cgN : CGEntryNode) : Node = {
    val node = newIDDGEntryNode(cgN).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGCenterNode(cgN : CGEntryNode) : Node =
    pool(newIDDGEntryNode(cgN))
    
  protected def newIDDGEntryNode(cgN : CGEntryNode) =
    IDDGEntryNode(cgN)
}

sealed abstract class IDDGNode(cgN : CGNode) extends InterProceduralNode(cgN.getContext) {
  def getCGNode = cgN
  def getOwner = cgN.getOwner
  override def getContext = cgN.getContext
}

abstract class IDDGVirtualNode(cgN : CGNode) extends IDDGNode(cgN) 

abstract class IDDGLocNode(cgN : CGLocNode) extends IDDGNode(cgN) {
  def getLocUri = cgN.getLocUri
  def getLocIndex : Int = cgN.getLocIndex
}

abstract class IDDGInvokeNode(cgN : CGInvokeNode) extends IDDGLocNode(cgN) {
  def getCalleeSet = cgN.getCalleeSet
}

final case class IDDGNormalNode(cgN : CGNormalNode) extends IDDGLocNode(cgN) 

final case class IDDGEntryParamNode(cgN : CGEntryNode, position : Int) extends IDDGVirtualNode(cgN){
  var paramName : String = null
  def getVirtualLabel : String = "EntryParam:" + position
}

final case class IDDGCenterNode(cgN : CGCenterNode) extends IDDGVirtualNode(cgN){
  def getVirtualLabel : String = "Center"
}

final case class IDDGEntryNode(cgN : CGEntryNode) extends IDDGVirtualNode(cgN)

final case class IDDGExitParamNode(cgN : CGExitNode, position : Int) extends IDDGVirtualNode(cgN){
  var paramName : String = null
  def getVirtualLabel : String = "ExitParam:" + position
}

final case class IDDGVirtualBodyNode(cgN : CGCallNode) extends IDDGInvokeNode(cgN){
  var argNames : List[String] = null
  def getInvokeLabel : String = "VirtualBody"
}

final case class IDDGCallArgNode(cgN : CGCallNode, position : Int) extends IDDGInvokeNode(cgN){
  var argName : String = null
  def getInvokeLabel : String = "CallArg:" + position
}

final case class IDDGReturnArgNode(cgN : CGReturnNode, position : Int) extends IDDGInvokeNode(cgN){
  var argName : String = null
  def getInvokeLabel : String = "ReturnArg:" + position
}

final case class IDDGReturnVarNode(cgN : CGCallNode) extends IDDGInvokeNode(cgN){
  def getInvokeLabel : String = "ReturnVar"
}