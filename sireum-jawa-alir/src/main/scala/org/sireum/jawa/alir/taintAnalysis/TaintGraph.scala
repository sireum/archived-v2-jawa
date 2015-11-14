/*
Copyright (c) 2015-2016 Fengguo Wei, University of South Florida.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.taintAnalysis

import org.sireum.jawa.alir.interProcedural.InterProceduralGraph
import org.sireum.jawa.alir.interProcedural.InterProceduralNode
import org.sireum.util._

/**
 * @author fgwei
 */
class TaintGraph extends InterProceduralGraph[TaintNode]{
  private val sources: MSet[TaintNode] = msetEmpty
  def addSource(src: TaintNode) = {
    addNode(src)
    sources += src
  }
  
  def taintNodeExists(tf: TaintSlot): Boolean = {
    graph.containsVertex(newTaintNode(tf).asInstanceOf[TaintNode])
  }
  
  def getTaintNode(tf: TaintSlot): TaintNode =
    pool(newTaintNode(tf))
  
  protected def newTaintNode(tf: TaintSlot) =
    TaintNode(tf)
    
  def addTaintNode(tf: TaintSlot): TaintNode = {
    val node = newTaintNode(tf).asInstanceOf[TaintNode]
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  def addTaintEdge(srcSlot: TaintSlot, tarSlot: TaintSlot) = {
    if(!taintNodeExists(srcSlot)) addTaintNode(srcSlot)
    if(!taintNodeExists(tarSlot)) addTaintNode(tarSlot)
    addEdge(getTaintNode(srcSlot), getTaintNode(tarSlot))
  }
  
  def getSources = this.sources.toSet
}

case class TaintNode(tf: TaintSlot) extends InterProceduralNode(tf.context){
  
}