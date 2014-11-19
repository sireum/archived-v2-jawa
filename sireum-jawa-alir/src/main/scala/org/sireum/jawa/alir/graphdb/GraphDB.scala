package org.sireum.jawa.alir.graphdb

import com.tinkerpop.blueprints.impls.orient.OrientGraphFactory
import com.tinkerpop.blueprints.impls.orient.OrientGraph
import org.sireum.jawa.alir.interProcedural.InterProceduralNode
import com.tinkerpop.blueprints.Vertex
import org.sireum.jawa.alir.interProcedural.InterProceduralGraph
import org.sireum.alir.AlirEdge
import com.tinkerpop.blueprints.Edge

object GraphDB {
  
  val factory : OrientGraphFactory = new OrientGraphFactory("remote:localhost/testdb", "admin", "admin")
  
  def store[Node <: InterProceduralNode](g : InterProceduralGraph[Node]) = {
    val graph : OrientGraph = factory.getTx()
    try {
      startTransactions[Node](g, graph)
    } finally {
      graph.shutdown()
    }
  }
  
  def startTransactions[Node <: InterProceduralNode](g : InterProceduralGraph[Node], graph : OrientGraph) = {
    try{
      g.nodes.foreach{
        node =>
          insertNode(graph, node)
      }
      g.edges.foreach{
        edge =>
          insertEdge(graph, edge)
      }
      graph.commit()
    } catch {
      case e : Exception =>
        e.printStackTrace()
        graph.rollback()
    }
  }
  
  def insertNode[Node <: InterProceduralNode](graph : OrientGraph, node : Node) : Vertex = {
    val vex : Vertex = graph.addVertex("class:" + node.getContext.toString(), Nil:_*)
    vex.setProperty("code", node.getContext.toString())
    println("id:" + vex.getId())
    println("prop:" + vex.getProperty("code"))
    vex
  }
  
  def insertEdge[Node <: InterProceduralNode](graph : OrientGraph, e : AlirEdge[Node]) : Edge = {
    val src = graph.getVertex("class:" + e.source.getContext.toString())
    val dst = graph.getVertexByKey("code", e.target.getContext.toString())
    println("src->" + src)
    println("dst->" + dst)
    val edge : Edge = graph.addEdge(null, src, dst, "icfg")
    edge
  }
  
}