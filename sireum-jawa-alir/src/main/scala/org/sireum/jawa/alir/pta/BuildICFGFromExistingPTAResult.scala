/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa.alir.pta

import org.sireum.jawa.alir.controlFlowGraph.ICFGNode
import org.sireum.jawa.alir.controlFlowGraph.InterproceduralControlFlowGraph
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.util._
import org.sireum.jawa._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.controlFlowGraph.ICFGCallNode
import org.sireum.jawa.alir.interProcedural.Callee
import org.sireum.jawa.alir.interProcedural.StaticCallee
import org.sireum.jawa.alir.util.CallHandler
import org.sireum.jawa.alir.controlFlowGraph.ICFGInvokeNode
import org.sireum.jawa.alir.interProcedural.InstanceCallee

object BuildICFGFromExistingPTAResult {
  
  def apply = build _
  
  type N = ICFGNode
  
  private def build(global: Global, entryPoints: ISet[Signature], ptaresult: PTAResult): InterProceduralDataFlowGraph = {
    val icfg = new InterproceduralControlFlowGraph[N]
    entryPoints foreach {
      ep =>
        val epmopt = global.getMethod(ep)
        epmopt match {
          case Some(epm) => 
            if(epm.isConcrete)
              doBuild(global, epm, icfg, ptaresult)
          case None =>
        }
    }
    InterProceduralDataFlowGraph(icfg, ptaresult)
  }
  
  private def doBuild(
      global: Global,
      ep: JawaMethod,
      icfg: InterproceduralControlFlowGraph[N], 
      ptaresult: PTAResult): Unit = {
    val context: Context = new Context
    val worklist: MList[JawaMethod] = mlistEmpty :+ ep
    val processed: MSet[Signature] = msetEmpty
    while(!worklist.isEmpty) {
      val currentMethod = worklist.remove(0)
      val nodes = icfg.collectCfgToBaseGraph(currentMethod, context.copy)
      nodes.foreach {
        node =>
          node match {
            case icn: ICFGCallNode =>
              val callercontext = icn.context
              val calleesig = icn.getCalleeSig
              val callType = icn.getCallType
              val calleeSet: MSet[Callee] = msetEmpty
              callType match {
                case "static" =>
                  CallHandler.getStaticCalleeMethod(global, calleesig) match {
                    case Some(callee) => calleeSet += StaticCallee(callee.getSignature)
                    case None =>
                  }
                case _ =>
                  val inss = ptaresult.getPTSMap(icn.context).getOrElse(VarSlot(icn.argNames(0), false, true), isetEmpty)
                  callType match {
                    case "direct" =>
                      CallHandler.getDirectCalleeMethod(global, calleesig) match {
                        case Some(callee) => calleeSet ++= inss.map(InstanceCallee(callee.getSignature, _))
                        case None =>
                      }
                    case "super" =>
                      CallHandler.getSuperCalleeMethod(global, calleesig) match {
                        case Some(callee) => calleeSet ++= inss.map(InstanceCallee(callee.getSignature, _))
                        case None =>
                      }
                    case "virtual" | _ =>
                      inss.map {
                        ins =>
                          ins match {
                            case un if un.isUnknown => 
                              val p = CallHandler.getUnknownVirtualCalleeMethods(global, un.typ, calleesig.getSubSignature)
                              calleeSet ++= p.map(callee => InstanceCallee(callee.getSignature, ins))
                            case _ => 
                              val p = CallHandler.getVirtualCalleeMethod(global, ins.typ, calleesig.getSubSignature)
                              calleeSet ++= p.map(callee => InstanceCallee(callee.getSignature, ins))
                          }
                      }
                  }
              }
              var bypassflag = false
              calleeSet.foreach(
                callee => {
                  icfg.getCallGraph.addCall(icn.getOwner, callee.callee)
                  val calleeProc = global.getMethod(callee.callee)
                  if(calleeProc.isDefined && !PTAScopeManager.shouldBypass(calleeProc.get.getDeclaringClass) && calleeProc.get.isConcrete) {
                    extendGraphWithConstructGraph(calleeProc.get, icn.context.copy, icfg)
                  } else {
                    bypassflag = true
                  }
                }
              )
              if(calleeSet.isEmpty) bypassflag = true
              val callNode = icfg.getICFGCallNode(icn.context).asInstanceOf[ICFGInvokeNode]
              callNode.addCallees(calleeSet.toSet)
              val returnNode = icfg.getICFGReturnNode(icn.context).asInstanceOf[ICFGInvokeNode]
              returnNode.addCallees(calleeSet.toSet)
              if(!bypassflag){
                icfg.deleteEdge(callNode, returnNode)
              }
            case _ =>
          }
      }
    }
  }
  
  private def extendGraphWithConstructGraph(calleeProc: JawaMethod, 
      callerContext: Context, 
      icfg: InterproceduralControlFlowGraph[N]) = {
    icfg.collectCfgToBaseGraph(calleeProc, callerContext.copy)
    icfg.extendGraph(calleeProc.getSignature, callerContext.copy)
  }
}
