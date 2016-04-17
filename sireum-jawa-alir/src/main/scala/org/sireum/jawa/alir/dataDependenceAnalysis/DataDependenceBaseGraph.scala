/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa.alir.dataDependenceAnalysis

import org.sireum.jawa.alir.interProcedural.InterProceduralGraph
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.jawa.alir.interProcedural.InterProceduralNode
import org.sireum.util._
import org.sireum.jawa.alir.Context

/**
 * @author fgwei
 */
trait DataDependenceBaseGraph[Node <: IDDGNode] extends InterProceduralGraph[Node] {
  def entryNode: Node
  def icfg: InterproceduralControlFlowGraph[ICFGNode]
  
  def findDefSite(defSite: Context, isRet: Boolean = false): Option[Node] = {
    val icfgN = {
      if(this.icfg.icfgNormalNodeExists(defSite)) this.icfg.getICFGNormalNode(defSite)
      else if(this.icfg.icfgCallNodeExists(defSite)) this.icfg.getICFGCallNode(defSite)
      else if(defSite.getLocUri == "L0000") this.icfg.entryNode
      else throw new RuntimeException("Cannot find node: " + defSite)
    }
    if(icfgN.isInstanceOf[ICFGNormalNode] && iddgNormalNodeExists(icfgN.asInstanceOf[ICFGNormalNode])) Some(getIDDGNormalNode(icfgN.asInstanceOf[ICFGNormalNode]))
    else if(icfgN.isInstanceOf[ICFGCallNode] && isRet && iddgReturnVarNodeExists(icfgN.asInstanceOf[ICFGCallNode])) Some(getIDDGReturnVarNode(icfgN.asInstanceOf[ICFGCallNode]))
    else if(icfgN.isInstanceOf[ICFGCallNode] && iddgVirtualBodyNodeExists(icfgN.asInstanceOf[ICFGCallNode])) Some(getIDDGVirtualBodyNode(icfgN.asInstanceOf[ICFGCallNode]))
    else if(icfgN == this.icfg.entryNode) Some(this.entryNode)
    else None
  }
  
  def findVirtualBodyDefSite(defSite: Context): Option[Node] = {
    val icfgN = if(this.icfg.icfgCallNodeExists(defSite)) Some(this.icfg.getICFGCallNode(defSite)) else None
    icfgN match {
      case Some(n: ICFGCallNode) =>
        if(iddgVirtualBodyNodeExists(n)) 
          Some(getIDDGVirtualBodyNode(n))
        else None
      case _ => None
    }
  }
  
  def findDefSite(defSite: Context, position: Int): Node = {
    val icfgN = {
      if(this.icfg.icfgCallNodeExists(defSite)) this.icfg.getICFGCallNode(defSite)
      else if(this.icfg.icfgReturnNodeExists(defSite)) this.icfg.getICFGReturnNode(defSite)
      else if(this.icfg.icfgEntryNodeExists(defSite)) this.icfg.getICFGEntryNode(defSite)
      else if(this.icfg.icfgExitNodeExists(defSite)) this.icfg.getICFGExitNode(defSite)
      else throw new RuntimeException("Cannot find node: " + defSite)
    }
    if(icfgN.isInstanceOf[ICFGCallNode] && iddgCallArgNodeExists(icfgN.asInstanceOf[ICFGCallNode], position)) getIDDGCallArgNode(icfgN.asInstanceOf[ICFGCallNode], position)
    else if(icfgN.isInstanceOf[ICFGReturnNode] && iddgReturnArgNodeExists(icfgN.asInstanceOf[ICFGReturnNode], position)) getIDDGReturnArgNode(icfgN.asInstanceOf[ICFGReturnNode], position)
    else if(icfgN.isInstanceOf[ICFGEntryNode] && iddgEntryParamNodeExists(icfgN.asInstanceOf[ICFGEntryNode], position)) getIDDGEntryParamNode(icfgN.asInstanceOf[ICFGEntryNode], position)
    else if(icfgN.isInstanceOf[ICFGExitNode] && iddgExitParamNodeExists(icfgN.asInstanceOf[ICFGExitNode], position)) getIDDGExitParamNode(icfgN.asInstanceOf[ICFGExitNode], position)
    else throw new RuntimeException("Cannot find node: " + defSite + ":" + position)
  }
  
  def iddgEntryParamNodeExists(icfgN: ICFGEntryNode, position: Int): Boolean = {
    graph.containsVertex(newIDDGEntryParamNode(icfgN, position).asInstanceOf[Node])
  }

  def addIDDGEntryParamNode(icfgN: ICFGEntryNode, position: Int): Node = {
    val node = newIDDGEntryParamNode(icfgN, position).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGEntryParamNode(icfgN: ICFGEntryNode, position: Int): Node =
    pool(newIDDGEntryParamNode(icfgN, position))
    
  protected def newIDDGEntryParamNode(icfgN: ICFGEntryNode, position: Int) =
    IDDGEntryParamNode(icfgN, position)
  
  def iddgExitParamNodeExists(icfgN: ICFGExitNode, position: Int): Boolean = {
    graph.containsVertex(newIDDGExitParamNode(icfgN, position).asInstanceOf[Node])
  }

  def addIDDGExitParamNode(icfgN: ICFGExitNode, position: Int): Node = {
    val node = newIDDGExitParamNode(icfgN, position).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGExitParamNode(icfgN: ICFGExitNode, position: Int): Node =
    pool(newIDDGExitParamNode(icfgN, position))
    
  protected def newIDDGExitParamNode(icfgN: ICFGExitNode, position: Int) =
    IDDGExitParamNode(icfgN, position)
    
  def iddgCallArgNodeExists(icfgN: ICFGCallNode, position: Int): Boolean = {
    graph.containsVertex(newIDDGCallArgNode(icfgN, position).asInstanceOf[Node])
  }

  def addIDDGCallArgNode(icfgN: ICFGCallNode, position: Int): Node = {
    val node = newIDDGCallArgNode(icfgN, position).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGCallArgNode(icfgN: ICFGCallNode, position: Int): Node =
    pool(newIDDGCallArgNode(icfgN, position))
    
  def getIDDGCallArgNodes(icfgN: ICFGCallNode): IList[Node] = {
    val result: MList[Node] = mlistEmpty
    var position = 0
    while(iddgCallArgNodeExists(icfgN, position)){
      result += pool(newIDDGCallArgNode(icfgN, position))
      position += 1
    }
    result.toList
  }
    
  protected def newIDDGCallArgNode(icfgN: ICFGCallNode, position: Int) = IDDGCallArgNode(icfgN, position)
    
  def iddgReturnArgNodeExists(icfgN: ICFGReturnNode, position: Int): Boolean = {
    graph.containsVertex(newIDDGReturnArgNode(icfgN, position).asInstanceOf[Node])
  }

  def addIDDGReturnArgNode(icfgN: ICFGReturnNode, position: Int): Node = {
    val node = newIDDGReturnArgNode(icfgN, position).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGReturnArgNode(icfgN: ICFGReturnNode, position: Int): Node =
    pool(newIDDGReturnArgNode(icfgN, position))
    
  protected def newIDDGReturnArgNode(icfgN: ICFGReturnNode, position: Int) = IDDGReturnArgNode(icfgN, position)
    
  def iddgReturnVarNodeExists(icfgN: ICFGCallNode): Boolean = {
    graph.containsVertex(newIDDGReturnVarNode(icfgN).asInstanceOf[Node])
  }

  def addIDDGReturnVarNode(icfgN: ICFGCallNode): Node = {
    val node = newIDDGReturnVarNode(icfgN).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGReturnVarNode(icfgN: ICFGCallNode): Node =
    pool(newIDDGReturnVarNode(icfgN))
    
  protected def newIDDGReturnVarNode(icfgN: ICFGCallNode) =
    IDDGReturnVarNode(icfgN)
    
  def iddgVirtualBodyNodeExists(icfgN: ICFGCallNode): Boolean = {
    graph.containsVertex(newIDDGVirtualBodyNode(icfgN).asInstanceOf[Node])
  }
  
  def addIDDGVirtualBodyNode(icfgN: ICFGCallNode): Node = {
    val node = newIDDGVirtualBodyNode(icfgN).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGVirtualBodyNode(icfgN: ICFGCallNode): Node =
    pool(newIDDGVirtualBodyNode(icfgN))
    
  protected def newIDDGVirtualBodyNode(icfgN: ICFGCallNode) =
    IDDGVirtualBodyNode(icfgN)
    
  def iddgNormalNodeExists(icfgN: ICFGNormalNode): Boolean = {
    graph.containsVertex(newIDDGNormalNode(icfgN).asInstanceOf[Node])
  }
  
  def addIDDGNormalNode(icfgN: ICFGNormalNode): Node = {
    val node = newIDDGNormalNode(icfgN).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGNormalNode(icfgN: ICFGNormalNode): Node =
    pool(newIDDGNormalNode(icfgN))
    
  protected def newIDDGNormalNode(icfgN: ICFGNormalNode) =
    IDDGNormalNode(icfgN)
    
  def iddgCenterNodeExists(icfgN: ICFGCenterNode): Boolean = {
    graph.containsVertex(newIDDGCenterNode(icfgN).asInstanceOf[Node])
  }
  
  def addIDDGCenterNode(icfgN: ICFGCenterNode): Node = {
    val node = newIDDGCenterNode(icfgN).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGCenterNode(icfgN: ICFGCenterNode): Node =
    pool(newIDDGCenterNode(icfgN))
    
  protected def newIDDGCenterNode(icfgN: ICFGCenterNode) =
    IDDGCenterNode(icfgN)
    
  def iddgEntryNodeExists(icfgN: ICFGEntryNode): Boolean = {
    graph.containsVertex(newIDDGEntryNode(icfgN).asInstanceOf[Node])
  }
  
  def addIDDGEntryNode(icfgN: ICFGEntryNode): Node = {
    val node = newIDDGEntryNode(icfgN).asInstanceOf[Node]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }

  def getIDDGCenterNode(icfgN: ICFGEntryNode): Node =
    pool(newIDDGEntryNode(icfgN))
    
  protected def newIDDGEntryNode(icfgN: ICFGEntryNode) =
    IDDGEntryNode(icfgN)
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
sealed abstract class IDDGNode(icfgN: ICFGNode) extends InterProceduralNode(icfgN.getContext) {
  def getICFGNode = icfgN
  def getOwner = icfgN.getOwner
  def getPosition: Option[Int]
//  def getCode: String = icfgN.getCode
  override def getContext = icfgN.getContext
  override def toString = icfgN.toString()
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
abstract class IDDGVirtualNode(icfgN: ICFGNode) extends IDDGNode(icfgN) {
  def getPosition: Option[Int] = None
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
abstract class IDDGLocNode(icfgN: ICFGLocNode) extends IDDGNode(icfgN) {
  def getLocUri = icfgN.getLocUri
  def getLocIndex: Int = icfgN.getLocIndex
  def getPosition: Option[Int] = None
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
abstract class IDDGInvokeNode(icfgN: ICFGInvokeNode) extends IDDGLocNode(icfgN) {
  def getCalleeSet = icfgN.getCalleeSet
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class IDDGNormalNode(icfgN: ICFGNormalNode) extends IDDGLocNode(icfgN) 

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class IDDGEntryParamNode(icfgN: ICFGEntryNode, position: Int) extends IDDGVirtualNode(icfgN){
  var paramName: String = null
  def getVirtualLabel: String = "EntryParam:" + position
  override def getPosition: Option[Int] = Some(position)
  override def toString: String = icfgN.toString() + "p" + position
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class IDDGCenterNode(icfgN: ICFGCenterNode) extends IDDGVirtualNode(icfgN){
  def getVirtualLabel: String = "Center"
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class IDDGEntryNode(icfgN: ICFGEntryNode) extends IDDGVirtualNode(icfgN)

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class IDDGExitParamNode(icfgN: ICFGExitNode, position: Int) extends IDDGVirtualNode(icfgN){
  var paramName: String = null
  def getVirtualLabel: String = "ExitParam:" + position
  override def getPosition: Option[Int] = Some(position)
  override def toString: String = icfgN.toString() + "p" + position
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class IDDGVirtualBodyNode(icfgN: ICFGCallNode) extends IDDGInvokeNode(icfgN){
  var argNames: List[String] = null
  def getInvokeLabel: String = "VirtualBody"
  override def toString: String = getInvokeLabel + "@" + icfgN.context
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class IDDGCallArgNode(icfgN: ICFGCallNode, position: Int) extends IDDGInvokeNode(icfgN){
  var argName: String = null
  def getInvokeLabel: String = "CallArg:" + position
  override def getPosition: Option[Int] = Some(position)
  override def toString: String = icfgN.toString() + "p" + position
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class IDDGReturnArgNode(icfgN: ICFGReturnNode, position: Int) extends IDDGInvokeNode(icfgN){
  var argName: String = null
  def getInvokeLabel: String = "ReturnArg:" + position
  override def getPosition: Option[Int] = Some(position)
  override def toString: String = icfgN.toString() + "p" + position
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class IDDGReturnVarNode(icfgN: ICFGCallNode) extends IDDGInvokeNode(icfgN){
  var retVarName: String = null
  def getInvokeLabel: String = "ReturnVar"
  override def toString: String = getInvokeLabel + "@" + icfgN.context
}
