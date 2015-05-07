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
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.util.SignatureParser
import com.tinkerpop.blueprints.Vertex
import org.sireum.jawa.alir.interProcedural.Callee

class CallGraph {
  /**
   * map from methods to it's callee methods
   * map from caller sig to callee sigs
   */
  private val callMap: MMap[String, ISet[String]] = mmapEmpty
  
  def addCall(from: String, to: String) = this.callMap(from) = this.callMap.getOrElse(from, isetEmpty) + to
  def addCalls(from: String, to: ISet[String]) = this.callMap(from) = this.callMap.getOrElse(from, isetEmpty) ++ to
  
  def getCallMap: IMap[String, ISet[String]] = this.callMap.toMap

  def getReachableMethods(procs: Set[String]): Set[String] = {
    calculateReachableMethods(procs, isetEmpty) ++ procs
  }
  
  private def calculateReachableMethods(procs: Set[String], processed: Set[String]): Set[String] = {
    if(procs.isEmpty) Set()
    else
      procs.map{
        proc =>
          if(processed.contains(proc)){
            Set[String]()
          } else {
            val callees = this.callMap.getOrElse(proc, isetEmpty)
            callees ++ calculateReachableMethods(callees, processed + proc)
          }
      }.reduce((s1, s2) => s1 ++ s2)
  }
  
  private def addNode(header: String, tg: TinkerGraph, node: CGNode): Vertex = {
    var v = tg.getVertex(node.hashCode())
    if(v == null){
      v = tg.addVertex(node.hashCode())
      v.setProperty("method", node.getMethodName)
      v.setProperty("class", node.getClassName)
      v.setProperty("returnType", node.getReturnType)
      for(i <- 0 to node.getParamTypes.size - 1){
        v.setProperty("param" + i + "Type", node.getParamTypes(i))
      }
      v.setProperty("type", node.getType)
      v.setProperty("location", node.getLocation)
      if(header != null && !header.isEmpty())
        v.setProperty("header", header)
    }
    v
  }
  
  def toSimpleCallGraph(header: String, outpath: String, format: String) = {
    val fm = format match {
      case "GraphML" => TinkerGraph.FileType.GRAPHML
      case "GML" => TinkerGraph.FileType.GML
      case _ => throw new RuntimeException("Given format " + format + " does not supported!")
    }
    val scg = new TinkerGraph(outpath, fm)
    
    this.callMap.foreach {
      case (caller, callees) =>
        val callerContext = new Context(0)
        callerContext.setContext(caller, caller)
        val callerNode = CGSimpleCallNode(callerContext)
        val callerV = addNode(header, scg, callerNode)
        callees foreach {
          case callee =>
            val calleeContext = new Context(0)
            calleeContext.setContext(callee, callee)
            val calleeNode = CGSimpleCallNode(calleeContext)
            val calleeV = addNode(header, scg, calleeNode)
            val e = scg.addEdge((callerV, calleeV).hashCode(), callerV, calleeV, "calls")
            e.setProperty("header", header)
        }
    }
    scg.shutdown()
  }
  
  def toDetailedCallGraph(header: String, icfg: InterproceduralControlFlowGraph[ICFGNode], outpath: String, format: String) = {
    val fm = format match {
      case "GraphML" => TinkerGraph.FileType.GRAPHML
      case "GML" => TinkerGraph.FileType.GML
      case _ => throw new RuntimeException("Given format " + format + " does not supported!")
    }
    val worklist: MList[ICFGNode] = mlistEmpty ++ icfg.nodes.toList
    while(!worklist.isEmpty) {
      val n = worklist.remove(0)
      n match{
        case cn: ICFGCallNode =>
          val rn = icfg.getICFGReturnNode(cn.context)
          icfg.addEdge(n, rn)
          icfg.predecessors(rn) foreach {
            pred =>
              if(pred.isInstanceOf[ICFGExitNode]){
                icfg.deleteEdge(pred, rn)
              }
          }
          val calleeSigs = cn.getCalleeSet.map(_.callee.getSignature)
          val hasCallees = icfg.successors(cn).filter { x => x.isInstanceOf[ICFGEntryNode] }.map {
            succ =>
              succ.asInstanceOf[ICFGEntryNode].getOwner
          }
          (calleeSigs -- hasCallees).foreach {
            sig =>
              val calleeContext = cn.getContext.copy.setContext(sig, "Entry")
              val calleeEntryNode = icfg.addICFGEntryNode(calleeContext)
              icfg.addEdge(cn, calleeEntryNode)
          }
        case _ =>
      }
    }
    val ns = icfg.nodes filter{
      n =>
        n match{
          case cn: ICFGCallNode => false
          case en: ICFGEntryNode => false
          case en: ICFGExitNode => false
          case _ => true
        }
    }
    ns foreach(icfg.compressByDelNode(_))
    val dcg = new TinkerGraph(outpath, fm)
    icfg.nodes foreach {
      n =>
        n match{
          case cn: ICFGCallNode =>
            val callees: ISet[Callee] = cn.getCalleeSet
            val calleesig: String = cn.getCalleeSig
            val source = CGDetailCallNode(calleesig, callees, cn.context)
            val sourceV = addNode(header, dcg, source)
            icfg.successors(cn).foreach { 
              s => 
                s match {
                  case sen: ICFGEntryNode =>
                    val target = CGEntryNode(sen.context)
                    val targetV = addNode(header, dcg, target)
                    val e = dcg.addEdge((sourceV, targetV).hashCode, sourceV, targetV, "calls")
                    e.setProperty("header", header)
                  case sen: ICFGExitNode =>
                    val target = CGExitNode(sen.context)
                    val targetV = addNode(header, dcg, target)
                    val e = dcg.addEdge((sourceV, targetV).hashCode, sourceV, targetV, "leadsto")
                    e.setProperty("header", header)
                  case scn: ICFGCallNode =>
                    val callees: ISet[Callee] = scn.getCalleeSet
                    val calleesig: String = scn.getCalleeSig
                    val target = CGDetailCallNode(calleesig, callees, scn.context)
                    val targetV = addNode(header, dcg, target)
                    val e = dcg.addEdge((sourceV, targetV).hashCode, sourceV, targetV, "leadsto")
                    e.setProperty("header", header)
                  case _ => throw new RuntimeException(s + " cannot be successor of " + cn + "!")
                }
            }
            
          case en: ICFGEntryNode =>
            val source = CGEntryNode(en.context)
            val sourceV = addNode(header, dcg, source)
            icfg.successors(en) foreach {
              case s =>
                s match {
                  case sen: ICFGExitNode =>
                    val target = CGExitNode(sen.context)
                    val targetV = addNode(header, dcg, target)
                    val e = dcg.addEdge((sourceV, targetV).hashCode, sourceV, targetV, "leadsto")
                    e.setProperty("header", header)
                  case scn: ICFGCallNode =>
                    val callees: ISet[Callee] = scn.getCalleeSet
                    val calleesig: String = scn.getCalleeSig
                    val target = CGDetailCallNode(calleesig, callees, scn.context)
                    val targetV = addNode(header, dcg, target)
                    val e = dcg.addEdge((sourceV, targetV).hashCode, sourceV, targetV, "leadsto")
                    e.setProperty("header", header)
                  case _ => throw new RuntimeException(s + " cannot be successor of " + en + "!")
                }
            }
          case en: ICFGExitNode =>
            val source = CGExitNode(en.context)
            val sourceV = addNode(header, dcg, source)
            icfg.successors(en) foreach {
              case s => // s should be only IcfgCallNode
                s match {
                  case cn: ICFGCallNode =>
                    val callees: ISet[Callee] = cn.getCalleeSet
                    val calleesig: String = cn.getCalleeSig
                    val target = CGDetailCallNode(calleesig, callees, cn.context)
                    val targetV = addNode(header, dcg, target)
                    val e = dcg.addEdge((sourceV, targetV).hashCode, sourceV, targetV, "return")
                    e.setProperty("header", header)
                  case _ => throw new RuntimeException(s + " cannot be successor of " + en + "!")
                }
                
            }
          case _ => throw new RuntimeException(n + " should not exist!")
        }
    }
    dcg.shutdown()
  }
}

sealed abstract class CGNode(context: Context) {
  def getID: String = this.hashCode().toLong.toString()
  def getMethodName: String = StringFormConverter.getMethodShortNameFromMethodSignature(context.getMethodSig)
  def getClassName: String = StringFormConverter.getClassNameFromMethodSignature(context.getMethodSig)
  def getReturnType: String = new SignatureParser(context.getMethodSig).getReturnType().name
  def getParamTypes: ISeq[String] = new SignatureParser(context.getMethodSig).getParamSig.getParameterTypes().map(_.name)
  def getType: String
  def getLocation: String = context.getCurrentLocUri
}

abstract class CGVirtualNode(context: Context) extends CGNode(context){
  override def toString: String = getID + ":" + getType
}

final case class CGEntryNode(context: Context) extends CGVirtualNode(context){
  def getType: String = "Entry"
}

final case class CGExitNode(context: Context) extends CGVirtualNode(context){
  def getType: String = "Exit"  
}

abstract class CGCallNode(context: Context) extends CGNode(context) {
  def getType: String = "Call"
}

final case class CGSimpleCallNode(context: Context) extends CGCallNode(context){
  override def toString: String = getID
}

final case class CGDetailCallNode(sig: String, callees: ISet[Callee], context: Context) extends CGCallNode(context){
  override def getMethodName: String = StringFormConverter.getMethodShortNameFromMethodSignature(sig)
  override def getClassName: String = StringFormConverter.getClassNameFromMethodSignature(sig)
  override def getReturnType: String = new SignatureParser(sig).getReturnType().name
  override def getParamTypes: ISeq[String] = new SignatureParser(sig).getParamSig.getParameterTypes().map(_.name)
  override def toString: String = getID + ":" + getType + "@" + getLocation
}