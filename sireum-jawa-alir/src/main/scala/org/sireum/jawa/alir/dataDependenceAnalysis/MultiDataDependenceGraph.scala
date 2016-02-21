/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.jawa.alir.dataDependenceAnalysis

import org.sireum.jawa.alir.interProcedural.InterProceduralGraph
import org.sireum.util._
import org.sireum.jawa.JawaClass
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.Signature

/**
 * @author fgwei
 */
class MultiDataDependenceGraph[Node <: IDDGNode] extends DataDependenceBaseGraph[Node] {
  val icfg: InterproceduralControlFlowGraph[ICFGNode] = new InterproceduralControlFlowGraph[ICFGNode]
  val encontext = new Context().setContext(new Signature("LMDDGEntry;.entry:()V"), "L0000")
  icfg.addEntryNode(icfg.addICFGEntryNode(encontext).asInstanceOf[ICFGEntryNode])
  val entryNode: Node = addIDDGEntryNode(icfg.entryNode.asInstanceOf[ICFGEntryNode])
  
  private val loadedSet: MSet[IDDGEntryNode] =msetEmpty
  
  def isLoaded(iddg: DataDependenceBaseGraph[Node]): Boolean = {
    loadedSet.contains(iddg.entryNode.asInstanceOf[IDDGEntryNode])
  }
  
  def addGraph(iddg: DataDependenceBaseGraph[Node]): Unit = {
    if(isLoaded(iddg)) return
    this.synchronized{
      loadedSet += iddg.entryNode.asInstanceOf[IDDGEntryNode]
      icfg.merge(iddg.icfg)
      this.pl ++= iddg.pool
      iddg.nodes.foreach(addNode(_))
      iddg.edges.foreach(addEdge(_))
    }
  }
}
