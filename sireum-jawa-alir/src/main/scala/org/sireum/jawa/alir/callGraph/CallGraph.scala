/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.callGraph

import org.sireum.util._
import java.io.Writer
import org.sireum.jawa.alir.interProcedural.InterProceduralGraph
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.interProcedural.InterProceduralNode
import org.sireum.jawa.alir.controlFlowGraph.InterproceduralControlFlowGraph
import org.sireum.jawa.alir.controlFlowGraph.ICFGNode
import org.sireum.jawa.alir.controlFlowGraph.ICFGCallNode
import org.sireum.jawa.alir.controlFlowGraph.ICFGEntryNode
import org.sireum.jawa.alir.controlFlowGraph.ICFGExitNode
import org.sireum.jawa.alir.controlFlowGraph.ICFGReturnNode
import org.jgrapht.ext.VertexNameProvider
import org.jgrapht.ext.EdgeNameProvider

class CallGraph {
  /**
   * map from procedures to it's callee procedures
   * map from caller sig to callee sigs
   */
  private val callMap : MMap[String, ISet[String]] = mmapEmpty
  
  def addCall(from : String, to : String) = this.callMap(from) = this.callMap.getOrElse(from, isetEmpty) + to
  def addCalls(from : String, to : ISet[String]) = this.callMap(from) = this.callMap.getOrElse(from, isetEmpty) ++ to
  
  def getCallMap : IMap[String, ISet[String]] = this.callMap.toMap

  def getReachableProcedures(procs : Set[String]) : Set[String] = {
    calculateReachableProcedures(procs, isetEmpty) ++ procs
  }
  
  private def calculateReachableProcedures(procs : Set[String], processed : Set[String]) : Set[String] = {
    if(procs.isEmpty) Set()
    else
      procs.map{
        proc =>
          if(processed.contains(proc)){
            Set[String]()
          } else {
            val callees = this.callMap.getOrElse(proc, isetEmpty)
            callees ++ calculateReachableProcedures(callees, processed + proc)
          }
      }.reduce((s1, s2) => s1 ++ s2)
  }
  
  def toSimpleCallGraph : SimpleCallGraph[CGSimpleCallNode] = {
    val scg = new SimpleCallGraph[CGSimpleCallNode]
    this.callMap.foreach {
      case (caller, callees) =>
        val callerContext = new Context(0)
        callerContext.setContext(caller, caller)
        val callerNode = CGSimpleCallNode(callerContext)
        scg.addNode(callerNode)
        callees foreach {
          case callee =>
            val calleeContext = new Context(0)
            calleeContext.setContext(callee, callee)
            val calleeNode = CGSimpleCallNode(calleeContext)
            scg.addNode(calleeNode)
            scg.addEdge(callerNode, calleeNode)
        }
    }
    scg
  }
  
  def toDetailedCallGraph(icfg : InterproceduralControlFlowGraph[ICFGNode]) : DetailedCallGraph[CGNode] = {
    icfg.nodes foreach {
      n =>
        n match{
          case cn : ICFGCallNode =>
            val rn = icfg.getICFGReturnNode(cn.context)
            icfg.addEdge(n, rn)
            icfg.predecessors(rn) foreach {
              pred =>
                if(pred.isInstanceOf[ICFGExitNode]){
                  icfg.deleteEdge(pred, rn)
                  icfg.addEdge(pred, n)
                }
            }
          case _ =>
        }
    }
    val ns = icfg.nodes filter{
      n =>
        n match{
          case cn : ICFGCallNode => false
          case en : ICFGEntryNode => false
          case en : ICFGExitNode => false
          case _ => true
        }
    }
    ns foreach(icfg.compressByDelNode(_))
    val dcg = new DetailedCallGraph[CGNode]
    icfg.nodes foreach {
      n =>
        n match{
          case cn : ICFGCallNode =>
            val source = CGDetailCallNode(cn.context)
            dcg.addNode(source)
            icfg.successors(cn).foreach { 
              s => 
                s match {
                  case sen : ICFGEntryNode =>
                    val target = CGEntryNode(sen.context)
                    dcg.addNode(target)
                    dcg.addEdge(source, target)
                  case sen : ICFGExitNode =>
                    val target = CGExitNode(sen.context)
                    dcg.addNode(target)
                    dcg.addEdge(source, target)
                  case scn : ICFGCallNode => // this is a model call case
                    val target = CGDetailCallNode(scn.context)
                    dcg.addNode(target)
                    dcg.addEdge(source, target)
                    val callees = cn.getCalleeSet
                    callees.foreach{
                      callee =>
                        val calleeContext = cn.getContext.setContext(callee.callee.getSignature, callee.callee.getSignature)
                        val calleeEntry = CGEntryNode(calleeContext)
                        val calleeExit = CGExitNode(calleeContext)
                        dcg.addNode(calleeEntry)
                        dcg.addNode(calleeExit)
                        dcg.addEdge(calleeEntry, calleeExit)
                        dcg.addEdge(source, calleeEntry)
                        dcg.addEdge(calleeExit, source)
                    }
                  case _ => throw new RuntimeException(s + " cannot be successor of " + cn + "!")
                }
            }
            
          case en : ICFGEntryNode =>
            val source = CGEntryNode(en.context)
            dcg.addNode(source)
            icfg.successors(en) foreach {
              case s =>
                s match {
                  case sen : ICFGExitNode =>
                    val target = CGExitNode(sen.context)
                    dcg.addNode(target)
                    dcg.addEdge(source, target)
                  case scn : ICFGCallNode =>
                    val target = CGDetailCallNode(scn.context)
                    dcg.addNode(target)
                    dcg.addEdge(source, target)
                  case _ => throw new RuntimeException(s + " cannot be successor of " + en + "!")
                }
            }
          case en : ICFGExitNode =>
            val source = CGExitNode(en.context)
            dcg.addNode(source)
            icfg.successors(en) foreach {
              case s => // s should be only IcfgCallNode
                s match {
                  case cn : ICFGCallNode =>
                    val target = CGDetailCallNode(cn.context)
                    dcg.addNode(target)
                    dcg.addEdge(source, target)
                  case _ => throw new RuntimeException(s + " cannot be successor of " + en + "!")
                }
                
            }
          case _ => throw new RuntimeException(n + " should not exist!")
        }
    }
    dcg
  }
}

class SimpleCallGraph[Node <: CGSimpleCallNode] extends InterProceduralGraph[Node] {
  
  override def addNode(node : Node) : Node = {
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  override val vIDProvider = new VertexNameProvider[Node]() {  
    def filterLabel(uri : String) = {
      uri.filter(_.isUnicodeIdentifierPart)  // filters out the special characters like '/', '.', '%', etc.  
    }
    def getVertexName(v : Node) : String = {
      v.toString
    }
  }
  
  override val eIDProvider = new EdgeNameProvider[Edge]() {
    def filterLabel(uri : String) = {
      uri.filter(_.isUnicodeIdentifierPart)  // filters out the special characters like '/', '.', '%', etc.  
    }
    def getEdgeName(e : Edge) : String = {
      e.source.toString() + "-Calls-" + e.target.toString()
    }
  }
}

class DetailedCallGraph[Node <: CGNode] extends InterProceduralGraph[Node]{
  
  override def addNode(node : Node) : Node = {
    val n =
      if (pool.contains(node)) pool(node)
      else {
        pl += (node -> node)
        node
      }
    graph.addVertex(n)
    n
  }
  
  override val vIDProvider = new VertexNameProvider[Node]() {
    def filterLabel(uri : String) = {
      uri.filter(_.isUnicodeIdentifierPart)  // filters out the special characters like '/', '.', '%', etc.  
    }
      
    def getVertexName(v : Node) : String = {
      v.getID
    }
  }
  
  
  val vLabelProvider = new VertexNameProvider[Node]() {
    def filterLabel(uri : String) = {
      uri.filter(_.isUnicodeIdentifierPart)  // filters out the special characters like '/', '.', '%', etc.  
    }
      
    def getVertexName(v : Node) : String = {
      "name=" + v.getName + " type=" + v.getType + " location=" + v.getLocation
    }
  }
  
  
  override val eIDProvider = new EdgeNameProvider[Edge]() {
    def filterLabel(uri : String) = {
      uri.filter(_.isUnicodeIdentifierPart)  // filters out the special characters like '/', '.', '%', etc.  
    }
    def getEdgeName(e : Edge) : String = {
      e.source.getID + "-to-" + e.target.getID
    }
  }
}

sealed abstract class CGNode(context : Context) extends InterProceduralNode(context) {
  def getID : String = this.hashCode().toLong.toString()
  def getName : String = context.getProcedureSig
  def getType : String
  def getLocation : String = context.getCurrentLocUri
}

final case class CGEntryNode(context : Context) extends CGNode(context){
  def getType : String = "Entry"
  override def toString : String = getID + ":" + getType
}

final case class CGExitNode(context : Context) extends CGNode(context){
  def getType : String = "Exit"
  override def toString : String = getID + ":" + getType
}

final case class CGSimpleCallNode(context : Context) extends CGNode(context){
  def getType : String = "Call"
  override def toString : String = getID
}

final case class CGDetailCallNode(context : Context) extends CGNode(context){
  def getType : String = "Call"
  override def toString : String = getID + ":" + getType + "@" + getLocation
}