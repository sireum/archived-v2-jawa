/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.compressedControlFlowGraph

import org.sireum.alir.AlirIntraProceduralNode
import org.sireum.alir.AlirIntraProceduralGraph
import org.sireum.util.ResourceUri

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait AlirIntraProceduralGraphExtra 
[Node <: AlirIntraProceduralNode, VirtualLabel]
    extends AlirIntraProceduralGraph[Node, VirtualLabel] {
  self =>

  def deleteNode(n : Node) = graph.removeVertex(n)
    
  def deleteEdge(source : Node, target : Node) : Edge =
    graph.removeEdge(getNode(source), getNode(target))

  def deleteEdge(e : Edge) = graph.removeEdge(e)

  def addNode(procUri : ResourceUri, locUri : ResourceUri, locIndex : Int) : Node = {
     val extdLocUri = procUri + "." + locUri
     val node = newNode(Option(extdLocUri), locIndex).asInstanceOf[Node]
     val n =
      if (pool.contains(node)) pool(node)
      else {
        pool(node) = node
        node
      }
     graph.addVertex(n)
     n
   }

  def getNode(procUri : ResourceUri, locUri : ResourceUri, locIndex : Int) : Node =
  {
    val extdLocUri =    procUri + "." + locUri
    pool(newNode(Option(extdLocUri), locIndex).asInstanceOf[Node])
  }
  
}


