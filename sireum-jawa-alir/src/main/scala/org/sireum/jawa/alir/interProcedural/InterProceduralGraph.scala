/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.interProcedural

import org.sireum.util._
import org.sireum.alir._
import org.jgrapht.EdgeFactory
import java.io.Writer
import org.jgrapht.ext.DOTExporter
import org.jgrapht.ext.VertexNameProvider
import scala.collection.mutable.SynchronizedMap
import scala.collection.mutable.HashMap
import org.sireum.jawa.alir.Context
import org.jgrapht.alg.DijkstraShortestPath
import org.jgrapht.graph.DirectedPseudograph

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait InterProceduralGraph[Node <: InterProceduralNode]
  extends AlirGraph[Node]
  with AlirEdgeAccesses[Node]
  with AlirSuccPredAccesses[Node]
  with Serializable {

  self=>
  
  protected val graph = new DirectedPseudograph(
    new EdgeFactory[Node, Edge] {
      def createEdge(source : Node, target : Node) =
        new AlirEdge(self, source, target)
    })
  
  def addNode(node : Node) : Node = {
    require(pool(node) eq node)
    graph.addVertex(node)
    node
  }
  
  def getNode(n : Node) : Node =
    pool(n)
  
  def deleteNode(node : Node) : Boolean =
    graph.removeVertex(node)

  def deleteEdge(source : Node, target : Node) : Edge =
    graph.removeEdge(getNode(source), getNode(target))

  def deleteEdge(e : Edge) = graph.removeEdge(e)
  
  protected val pl : MMap[InterProceduralNode, Node] = new HashMap[InterProceduralNode, Node] with SynchronizedMap[InterProceduralNode, Node]
  
  def pool : MMap[InterProceduralNode, Node] = pl
  
  protected val vlabelProvider = new VertexNameProvider[Node]() {
    
		def filterLabel(uri : String) = {
		  uri.filter(_.isUnicodeIdentifierPart)  // filters out the special characters like '/', '.', '%', etc.  
		}
	    
		def getVertexName(v : Node) : String = {
		  filterLabel(v.toString())
		}
  }
    
  def toDot(w : Writer) = {
    val de = new DOTExporter[Node, Edge](vlabelProvider, vlabelProvider, null)
    de.export(w, graph)
  }
  
  def findPath(srcNode : Node, tarNode : Node) : IList[Edge] = {
    import scala.collection.JavaConversions._
	  val path = DijkstraShortestPath.findPathBetween(this.graph, srcNode, tarNode)
	  if(path != null) path.toList
	  else null
	}
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
abstract class InterProceduralNode(context : Context) extends PropertyProvider with Serializable {
  val propertyMap = mlinkedMapEmpty[Property.Key, Any]
  def getContext = this.context
}