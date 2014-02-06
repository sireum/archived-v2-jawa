package org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis

import org.sireum.jawa._
import org.sireum.jawa.alir.interProcedural.controlFlowGraph._
import org.sireum.jawa.alir.Context
import org.jgrapht.alg.DijkstraShortestPath

class InterProceduralDataDependenceGraph[Node <: CGNode] extends InterproceduralControlFlowGraph[Node]{
  
	def addNodes(nodes : Set[Node]) = nodes.foreach(addNode(_))
	
	def initGraph(cg : InterproceduralControlFlowGraph[Node]) = {
	  this.pl.clear
	  this.pl ++= cg.pool
	  addNodes(cg.nodes.toSet)
//	  cg.nodes.foreach{
//	    node =>
//	      node match{
//	        case en : CGEntryNode =>
//	          val owner = en.getOwner
//	          val pnames = owner.getParamNames
//	          for(i <- 0 to pnames.size - 1){
//	            val node = IDDGEntryParamNode(en.context, i)
//	            addNode(node.asInstanceOf[Node])
//	          }
//	        case en : CGExitNode =>
//	          val owner = en.getOwner
//	          val pnames = owner.getParamNames
//	          for(i <- 0 to pnames.size - 1){
//	            val node = IDDGExitParamNode(en.context, i)
//	            addNode(node.asInstanceOf[Node])
//	          }
//	        case cn : CGCallNode =>
//	          val loc = cn.getOwner.getProcedureBody.location(cn.getLocIndex)
//	          loc match{
//	            case 
//	          }
//	        case rn : CGReturnNode =>
//	        case a => addNode(a)
//	      }
//	  }
	  this.entryN = getNode(cg.entryNode)
	  this.exitN = getNode(cg.exitNode)
	  this.centerN = getNode(cg.centerNode)
	}
	
	def findDefSite(defSite : Context) : Node = {
	  if(cgNormalNodeExists(defSite)) getCGNormalNode(defSite)
	  else if(cgReturnNodeExists(defSite)) getCGReturnNode(defSite)
	  else if(cgEntryNodeExists(defSite)) getCGEntryNode(defSite)
	  else if(defSite.toString == "(EntryPoint,L0000)") this.entryNode
	  else if(defSite.toString == "(Center,L0000)") this.centerNode
	  else throw new RuntimeException("Cannot find node: " + defSite)
	}
	
}

final case class IDDGEntryParamNode(context : Context, position : Int) extends CGVirtualNode(context){
  def getVirtualLabel : String = "EntryParam:" + position
}

final case class IDDGExitParamNode(context : Context, position : Int) extends CGVirtualNode(context){
  def getVirtualLabel : String = "ExitParam:" + position
}

final case class IDDGCallArgNode(context : Context, position : Int) extends CGInvokeNode(context){
  def getInvokeLabel : String = "CallArg:" + position
}

final case class IDDGReturnArgNode(context : Context, position : Int) extends CGInvokeNode(context){
  def getInvokeLabel : String = "ReturnArg:" + position
}

final case class IDDGReturnVarNode(context : Context) extends CGInvokeNode(context){
  def getInvokeLabel : String = "ReturnVar"
}