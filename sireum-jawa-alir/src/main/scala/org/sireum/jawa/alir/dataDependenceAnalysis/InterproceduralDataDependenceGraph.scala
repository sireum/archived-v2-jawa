/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.dataDependenceAnalysis

import org.sireum.jawa._
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.jawa.alir.Context
import org.sireum.pilar.ast._
import org.sireum.util._
import org.sireum.jawa.alir.interProcedural.InstanceCallee
import org.sireum.jawa.alir.interProcedural.InterProceduralNode
import org.sireum.jawa.alir.interProcedural.InterProceduralGraph
import org.sireum.jawa.util.ASTUtil

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class InterProceduralDataDependenceGraph[Node <: IDDGNode] extends DataDependenceBaseGraph[Node]{
	
//  protected var centerN: IDDGCenterNode = null
//  def centerNode: Node = this.centerN.asInstanceOf[Node]
  
  protected var entryN: IDDGEntryNode = null
  def entryNode: Node = this.entryN.asInstanceOf[Node]
  
  var icfg: InterproceduralControlFlowGraph[ICFGNode] = null
  
	def initGraph(global: Global, icfg: InterproceduralControlFlowGraph[ICFGNode]) = {
    this.icfg = icfg
	  icfg.nodes.foreach{
	    node =>
	      node match{
	        case en: ICFGEntryNode =>
	          val owner = global.getMethod(en.getOwner).get
            var position = 0
            owner.thisOpt match {
              case Some(t) =>
                val n = addIDDGEntryParamNode(en, position)
                n.asInstanceOf[IDDGEntryParamNode].paramName = t
                position += 1
              case None =>
            }
	          val pnames = owner.getParamNames
	          for(i <- 0 to pnames.size - 1){
	            val n = addIDDGEntryParamNode(en, position)
	            n.asInstanceOf[IDDGEntryParamNode].paramName = pnames(i)
	            position += 1
	          }
	        case en: ICFGExitNode =>
	          val owner = global.getMethod(en.getOwner).get
            var position = 0
            owner.thisOpt match {
              case Some(t) =>
                val n = addIDDGExitParamNode(en, position)
                n.asInstanceOf[IDDGExitParamNode].paramName = t
                position += 1
              case None =>
            }
	          val pnames = owner.getParamNames
            for(i <- 0 to pnames.size - 1){
              val n = addIDDGExitParamNode(en, position)
              n.asInstanceOf[IDDGExitParamNode].paramName = pnames(i)
              position += 1
            }
	        case en: ICFGCenterNode =>
	        case cn: ICFGCallNode =>
	          for(i <- 0 to cn.argNames.size - 1) {
	            val argName = cn.argNames(i)
	            val n = addIDDGCallArgNode(cn, i)
	            n.asInstanceOf[IDDGCallArgNode].argName = argName
	          }
            for(i <- 0 to cn.retNames.size - 1) {
              val retName = cn.retNames(i)
              val rn = addIDDGReturnVarNode(cn)
              rn.asInstanceOf[IDDGReturnVarNode].retVarName = retName
            }
            val succs = icfg.successors(cn)
            if(succs.exists{succ => succ.isInstanceOf[ICFGReturnNode]}){
	            val vn = addIDDGVirtualBodyNode(cn)
	            vn.asInstanceOf[IDDGVirtualBodyNode].argNames = cn.argNames
	          }
	        case rn: ICFGReturnNode =>
	          val loc =  global.getMethod(rn.getOwner).get.getBody.location(rn.getLocIndex)
	          val argNames: MList[String] = mlistEmpty
	          loc match{
	            case jumploc: JumpLocation =>
	              argNames ++= ASTUtil.getCallArgs(jumploc)
	            case _ =>
	          }
	          for(i <- 0 to (argNames.size - 1)){
	            val argName = argNames(i)
	            val n = addIDDGReturnArgNode(rn, i)
	            n.asInstanceOf[IDDGReturnArgNode].argName = argName
	          }
	        case nn: ICFGNormalNode => addIDDGNormalNode(nn)
	        case _ =>
	      }
	  }
//    this.centerN = addIDDGCenterNode(icfg.centerNode.asInstanceOf[ICFGCenterNode]).asInstanceOf[IDDGCenterNode]
    this.entryN = addIDDGEntryNode(icfg.entryNode.asInstanceOf[ICFGEntryNode]).asInstanceOf[IDDGEntryNode]
	}
}