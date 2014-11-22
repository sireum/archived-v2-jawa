package org.sireum.jawa.alir.graphdb

import com.tinkerpop.blueprints.impls.orient.OrientGraphFactory
import com.tinkerpop.blueprints.impls.orient.OrientGraph
import org.sireum.jawa.alir.interProcedural.InterProceduralNode
import com.tinkerpop.blueprints.Vertex
import org.sireum.jawa.alir.interProcedural.InterProceduralGraph
import org.sireum.alir.AlirEdge
import com.tinkerpop.blueprints.Edge
import org.sireum.util._
import org.sireum.jawa.alir.controlFlowGraph.CGNode
import org.sireum.jawa.alir.dataDependenceAnalysis.IDDGNode
import org.sireum.jawa.alir.dataDependenceAnalysis.InterProceduralDataDependenceGraph
import org.sireum.jawa.alir.controlFlowGraph.InterproceduralControlFlowGraph
import org.sireum.jawa.alir.interProcedural.InterProceduralDataFlowGraph

object GraphDB {
  
  val factory : OrientGraphFactory = new OrientGraphFactory("remote:129.130.10.134/TestDB", "amandroid", "Love Amandroid!")

  def storeIcfg[Node <: CGNode](owner : String, g : InterproceduralControlFlowGraph[Node]) = {
    /**
     * a cache map between context and it's vetex id
     */
    val cache : MMap[Int, Object] = mmapEmpty
    val graph : OrientGraph = factory.getTx()
    try {
      val vs = graph.getVertices("owner", owner)
      val es = graph.getEdges("owner", owner)
      import collection.JavaConversions._
      vs.foreach(_.remove())
      startTransactions_icfg[Node](owner, g, graph, cache)
    } finally {
      graph.shutdown()
    }
  }
  
  private def startTransactions_icfg[Node <: CGNode](owner : String, g : InterproceduralControlFlowGraph[Node], graph : OrientGraph, cache : MMap[Int, Object]) = {
    try{
      g.nodes.foreach{
        node =>
          insertNode_icfg(owner, graph, node, cache)
      }
      g.edges.foreach{
        edge =>
          insertEdge_icfg(owner, graph, edge, cache)
      }
      graph.commit()
    } catch {
      case e : Exception =>
        e.printStackTrace()
        graph.rollback()
    }
  }
  
  private def insertNode_icfg[Node <: CGNode](owner : String, graph : OrientGraph, node : Node, cache : MMap[Int, Object]) : Vertex = {
    val vex : Vertex = graph.addVertex("class:ICFGNode", Nil:_*)
    vex.setProperty("owner", owner)
    vex.setProperty("context", node.getContext.toString())
    vex.setProperty("code", node.asInstanceOf[CGNode].getCode)
    cache += (node.hashCode() -> vex.getId())
    vex
  }
  
  private def insertEdge_icfg[Node <: CGNode](owner : String, graph : OrientGraph, e : AlirEdge[Node], cache : MMap[Int, Object]) : Edge = {
    val srcid = cache.getOrElse(e.source.hashCode(), throw new RuntimeException("Could not find vetex of node: " + e.source.getContext))
    val dstid = cache.getOrElse(e.target.hashCode(), throw new RuntimeException("Could not find vetex of node: " + e.target.getContext))
    val src = graph.getVertex(srcid)
    val dst = graph.getVertex(dstid)
    val edge : Edge = graph.addEdge("class:ICFGEdge", src, dst, null)
    edge.setProperty("owner", owner)
    edge
  }
  
  def storeIdfg(owner : String, g : InterProceduralDataFlowGraph) = {
    /**
     * a cache map between context and it's vetex id
     */
    val cache : MMap[Int, Object] = mmapEmpty
    val graph : OrientGraph = factory.getTx()
    try {
      val vs = graph.getVertices("owner", owner)
      import collection.JavaConversions._
      vs.foreach(_.remove())
      startTransactions_idfg(owner, g, graph, cache)
    } finally {
      graph.shutdown()
    }
  }
  
  private def startTransactions_idfg(owner : String, g : InterProceduralDataFlowGraph, graph : OrientGraph, cache : MMap[Int, Object]) = {
    try{
      val icfg = g.icfg
      val rfaRes = g.summary
      icfg.nodes.foreach{
        node =>
          val vex = insertNode_icfg(owner, graph, node, cache)
          val facts = rfaRes.entrySet(node)
          val str : String = 
            if(!facts.isEmpty && facts != null) facts.map(f => "(" + f.s + "->" + f.v + ")\n").reduce[String]((v1, v2) => v1 + v2)
            else ""
          vex.setProperty("facts", str)
      }
      icfg.edges.foreach{
        edge =>
          insertEdge_icfg(owner, graph, edge, cache)
      }
      graph.commit()
    } catch {
      case e : Exception =>
        e.printStackTrace()
        graph.rollback()
    }
  }
  
}