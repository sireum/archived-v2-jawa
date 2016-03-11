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

import org.sireum.pilar.ast._
import org.sireum.util._
import org.sireum.jawa.JawaMethod
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ParamDefDesc
import org.sireum.alir.LocDefDesc
import org.sireum.alir.InitDefDesc
import org.sireum.jawa.alir.Context
import org.sireum.alir.DefDesc
import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.jawa.PilarAstHelper
import org.sireum.alir.AlirEdge
import org.sireum.jawa.alir.sideEffectAnalysis.InterProceduralSideEffectAnalysisResult
import org.sireum.jawa.alir.interProcedural.Callee
import org.sireum.jawa.alir.interProcedural.InstanceCallee
import org.sireum.jawa.alir.LibSideEffectProvider
import org.sireum.jawa.alir.pta.Instance
import java.io.PrintWriter
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.jawa.alir.pta.VarSlot
import org.sireum.jawa.alir.pta.FieldSlot
import org.sireum.jawa.alir.pta.ArraySlot
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.jawa.util.ASTUtil
import org.sireum.jawa.Global
import org.sireum.jawa.io.NoPosition
import org.sireum.jawa.FieldFQN
import org.sireum.jawa.JawaType
import org.sireum.jawa.JavaKnowledge

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait InterproceduralDataDependenceInfo{
  def getIddg: DataDependenceBaseGraph[InterproceduralDataDependenceAnalysis.Node]
  def getDependentPath(src: InterproceduralDataDependenceAnalysis.Node, dst: InterproceduralDataDependenceAnalysis.Node): IList[InterproceduralDataDependenceAnalysis.Edge]
  def isDependent(src: InterproceduralDataDependenceAnalysis.Node, dst: InterproceduralDataDependenceAnalysis.Node): Boolean
}

class DefaultInterproceduralDataDependenceInfo(iddg: DataDependenceBaseGraph[InterproceduralDataDependenceAnalysis.Node]) extends InterproceduralDataDependenceInfo{
  def getIddg: DataDependenceBaseGraph[InterproceduralDataDependenceAnalysis.Node] = iddg
  def getDependentPath(src: InterproceduralDataDependenceAnalysis.Node, dst: InterproceduralDataDependenceAnalysis.Node): IList[InterproceduralDataDependenceAnalysis.Edge] = {
    iddg.findPath(src, dst)
  }
  def isDependent(src: InterproceduralDataDependenceAnalysis.Node, dst: InterproceduralDataDependenceAnalysis.Node): Boolean = {
    getDependentPath(src, dst) != null
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object InterproceduralDataDependenceAnalysis {
  final val TITLE = "InterproceduralDataDependenceAnalysis"
  type Node = IDDGNode
  type Edge = AlirEdge[Node]
  
  def apply(global: Global, idfg: InterProceduralDataFlowGraph): InterproceduralDataDependenceInfo = build(global, idfg)

  def build(global: Global, idfg: InterProceduralDataFlowGraph): InterproceduralDataDependenceInfo = {
    val icfg = idfg.icfg
    val ptaresult = idfg.ptaresult
    val irdaResult = InterproceduralReachingDefinitionAnalysis(global, icfg)
    val iddg = new InterProceduralDataDependenceGraph[Node]
    iddg.initGraph(global, icfg)
    iddg.nodes.foreach {
      node =>
        val targetNodes: MSet[Node] = msetEmpty
        if(node != iddg.entryNode){
          node match {
            case en: IDDGEntryParamNode =>
              val icfgN = icfg.getICFGEntryNode(en.getContext)
              val icfgTarN = icfg.predecessors(icfgN)
              targetNodes ++= icfgTarN.map(n => iddg.findDefSite(n.getContext, en.position))
            case en: IDDGExitParamNode =>
              val icfgN = icfg.getICFGExitNode(en.getContext)
              val procName = en.paramName
              val irdaFacts = irdaResult(icfgN)
              targetNodes ++= searchRda(global, procName, en, irdaFacts, iddg)
            case cn: IDDGCallArgNode =>
              val icfgN = icfg.getICFGCallNode(cn.getContext)
              val irdaFacts = irdaResult(icfgN)
              targetNodes ++= processCallArg(global, cn, ptaresult, irdaFacts, iddg)
            case rn: IDDGReturnArgNode =>
              val icfgN = icfg.getICFGReturnNode(rn.getContext)
              val icfgTarN = icfg.predecessors(icfgN)
              icfgTarN.foreach{
                N =>
                  N match{
                    case cn: ICFGCallNode =>
                      targetNodes += iddg.findDefSite(cn.getContext, rn.position)
                    case en: ICFGExitNode =>
                      targetNodes += iddg.findDefSite(en.getContext, rn.position)
                    case _ =>
                  }
              }
            case rn: IDDGReturnVarNode =>
              val icfgN = icfg.getICFGReturnNode(rn.getContext)
              val icfgTarN = icfg.predecessors(icfgN)
              icfgTarN.foreach{
                N =>
                  N match{
                    case cn: ICFGCallNode =>
                      val retSlot = VarSlot(rn.retVarName, false, false)
                      val retInss = ptaresult.pointsToSet(retSlot, rn.getContext)
                      val idEntNs = iddg.getIDDGCallArgNodes(rn.icfgN).map(_.asInstanceOf[IDDGCallArgNode])
                      if(retInss.isEmpty) targetNodes ++= idEntNs
                      else {
                        val argInss = 
                          idEntNs.map {
                            n =>
                              val argSlot = VarSlot(n.argName, false, true)
                              ptaresult.getRelatedInstances(argSlot, n.getContext)
                          }
                        val poss = retInss.map {
                          ins =>
                            argInss.filter(_.contains(ins)) map (argInss.indexOf(_))
                        }.fold(ilistEmpty)(_ ++ _)
                        if(poss.isEmpty) targetNodes ++= idEntNs
                        else {
                          targetNodes ++= poss.map(pos => iddg.findDefSite(cn.getContext, pos))
                        }
                      }
                    case en: ICFGExitNode =>
                      val enPreds = icfg.predecessors(en)
                      enPreds foreach {
                        enPred =>
                          enPred match {
                            case nn: ICFGNormalNode =>
                              targetNodes ++= iddg.findDefSite(nn.getContext)
                            case _ =>
                          }
                      }
                    case _ =>
                  }
              }
            case vn: IDDGVirtualBodyNode =>
              val icfgN = vn.icfgN
              val idEntNs = iddg.getIDDGCallArgNodes(icfgN)
              targetNodes ++= idEntNs
              val irdaFacts = irdaResult(icfgN)
              targetNodes ++= processVirtualBody(global, vn, ptaresult, irdaFacts, iddg)
            case ln: IDDGNormalNode =>
              val icfgN = icfg.getICFGNormalNode(ln.getContext)
              val ownerProc = global.getMethod(ln.getOwner).get
              val loc = ownerProc.getBody.location(ln.getLocIndex)
              val irdaFacts = irdaResult(icfgN)
              targetNodes ++= processLocation(global, node, loc, ptaresult, irdaFacts, iddg)
            case a => 
          }
        }
        targetNodes.foreach(tn=>iddg.addEdge(node, tn))
    }
    global.reporter.echo(NoPosition, "[IDDG building done!]")
//    iddg.toDot(new PrintWriter(System.out))
    new DefaultInterproceduralDataDependenceInfo(iddg)
  }
  
  def processCallArg(
      global: Global,
      callArgNode: IDDGCallArgNode,
      ptaresult: PTAResult, 
      irdaFacts: ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], 
      iddg: InterProceduralDataDependenceGraph[Node]): ISet[Node] = {
    val result = msetEmpty[Node]
    result ++= searchRda(global, callArgNode.argName, callArgNode, irdaFacts, iddg)
    val argSlot = VarSlot(callArgNode.argName, false, true)
    val inss = ptaresult.pointsToSet(argSlot, callArgNode.getContext)
    inss.foreach(ins => result ++= iddg.findDefSite(ins.defSite))
    result.toSet
  }
  
  def processVirtualBody(
      global: Global,
      virtualBodyNode: IDDGVirtualBodyNode,
      ptaresult: PTAResult,
      irdaFacts: ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], 
      iddg: InterProceduralDataDependenceGraph[Node]): ISet[Node] = {
    val result = msetEmpty[Node]
    val calleeSet = virtualBodyNode.getCalleeSet
    calleeSet.foreach{
      callee =>
        val calleeSig = callee.callee
        if(global.isSystemLibraryClasses(calleeSig.getClassType) || global.isUserLibraryClasses(calleeSig.getClassType)) {
          val sideEffectResult = 
            if(LibSideEffectProvider.isDefined) LibSideEffectProvider.ipsear.result(calleeSig)
            else None
          for(i <- 0 to virtualBodyNode.argNames.size - 1) {
            val argSlot = VarSlot(virtualBodyNode.argNames(i), false, true)
            val argInss = ptaresult.pointsToSet(argSlot, virtualBodyNode.getContext)
            argInss.foreach (ins => result ++= iddg.findDefSite(ins.defSite))
            if(sideEffectResult.isDefined) {
              val readmap = sideEffectResult.get.readMap
              val position = i
              val fields = readmap.getOrElse(position, Set()) 
              argInss.foreach{
                case argIns =>
                  fields.foreach{
                    f => 
                      val fs = FieldSlot(argIns, f)
                      val argRelatedValue = ptaresult.getRelatedInstances(fs, virtualBodyNode.getContext)
                      argRelatedValue.foreach{ins => result ++= iddg.findDefSite(ins.defSite)}
                  }
              }
            } else if({
              val calleep = global.getMethod(calleeSig)
              if(calleep.isDefined) calleep.get.isConcrete
              else false
            }) {
              val argRelatedValue = ptaresult.getRelatedHeapInstances(argInss, virtualBodyNode.getContext)
              argRelatedValue.foreach{
                ins => 
                  result ++= iddg.findDefSite(ins.defSite)
              }
            }
          }
        }
    }
    result.toSet
  }

  def processLocation(
      global: Global, 
      node: Node, 
      loc: LocationDecl, 
      ptaresult: PTAResult, 
      irdaFacts: ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], 
      iddg: InterProceduralDataDependenceGraph[Node]): ISet[Node] = {
    var result = isetEmpty[Node]
    loc match{
      case al: ActionLocation =>
        al.action match {
          case aa: AssignAction =>
            val lhss = PilarAstHelper.getLHSs(aa)
            val rhss = PilarAstHelper.getRHSs(aa)
            val typ: Option[JawaType] = ASTUtil.getType(aa)
            result ++= processLHSs(global, node, lhss, ptaresult, irdaFacts, iddg)
            result ++= processRHSs(global, node, rhss, typ, ptaresult, irdaFacts, iddg)
          case _ =>
        }
      case jl: JumpLocation =>
        jl.jump match{
          case t: CallJump if t.jump.isEmpty =>
            val lhss = PilarAstHelper.getLHSs(t)
            val rhss = PilarAstHelper.getRHSs(t)
            result ++= processLHSs(global, node, lhss, ptaresult, irdaFacts, iddg)
            result ++= processRHSs(global, node, rhss, None, ptaresult, irdaFacts, iddg)
          case gj: GotoJump =>
          case rj: ReturnJump =>
            if (rj.exp.isDefined) {
              processExp(global, node, rj.exp.get, None, ptaresult, irdaFacts, iddg)
            }
          case ifj: IfJump =>
            for (ifThen <- ifj.ifThens) {
              processCondition(global, node, ifThen.cond, ptaresult, irdaFacts, iddg)
            }
          case sj: SwitchJump =>
            for (switchCase <- sj.cases) {
              processCondition(global, node, switchCase.cond, ptaresult, irdaFacts, iddg)
            }
        }
      case _ =>
    }
    result
  }

  def processLHSs(
      global: Global,
      node: Node, 
      lhss: Seq[Exp], 
      ptaresult: PTAResult, 
      irdaFacts: ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], 
      iddg: InterProceduralDataDependenceGraph[Node]): ISet[Node] = {
    var result = isetEmpty[Node]
    lhss.foreach{
      lhs =>
        lhs match {
          case ne: NameExp =>
          case ae: AccessExp =>
            val baseSlot = ae.exp match {
              case ne: NameExp => 
                result ++= searchRda(global, ne.name.name, node, irdaFacts, iddg)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
          case ie: IndexingExp =>
            val baseSlot = ie.exp match {
              case ine: NameExp =>
                result ++= searchRda(global, ine.name.name, node, irdaFacts, iddg)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
          case _=>
        }
    }
    result
  }

  def processRHSs(
      global: Global,
      node: Node, 
      rhss: Seq[Exp],
      typ: Option[JawaType],
      ptaresult: PTAResult, 
      irdaFacts: ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], 
      iddg: InterProceduralDataDependenceGraph[Node]): ISet[Node] = {
    var result = isetEmpty[Node]
    if(!rhss.isEmpty)
      result ++= rhss.map(processExp(global, node, _, typ, ptaresult, irdaFacts, iddg)).reduce(iunion[Node])
    result
  }

  def processExp(
      global: Global,
      node: Node,
      exp: Exp,
      typ: Option[JawaType],
      ptaresult: PTAResult, 
      irdaFacts: ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], 
      iddg: InterProceduralDataDependenceGraph[Node]): ISet[Node] = {
    var result = isetEmpty[Node]
    exp match {
      case ne: NameExp =>
        result ++= searchRda(global, ne.name.name, node, irdaFacts, iddg)
        val slot = VarSlot(ne.name.name, false, false)
        val value = ptaresult.pointsToSet(slot, node.getContext)
        value.foreach{
          ins =>
            result ++= iddg.findDefSite(ins.defSite)
        }
      case ae: AccessExp =>
        val fieldName: String = JavaKnowledge.getFieldNameFromFieldFQN(ae.attributeName.name)
        val baseSlot = ae.exp match {
          case ne: NameExp => 
            result ++= searchRda(global, ne.name.name, node, irdaFacts, iddg)
            VarSlot(ne.name.name, true, false)
          case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
        }
        val baseValue = ptaresult.pointsToSet(baseSlot, node.getContext)
        baseValue.foreach{
          ins =>
            result ++= iddg.findDefSite(ins.defSite)
            if(!ins.isNull) { // if(!ins.isInstanceOf[NullInstance] && !ins.isInstanceOf[UnknownInstance]){
              val fieldSlot = FieldSlot(ins, fieldName)
              val fieldValue = ptaresult.pointsToSet(fieldSlot, node.getContext)
              fieldValue.foreach(fIns => result ++= iddg.findDefSite(fIns.defSite))
            }
        }
      case ie: IndexingExp =>
        val baseSlot = ie.exp match {
          case ine: NameExp =>
            result ++= searchRda(global, ine.name.name, node, irdaFacts, iddg)
            VarSlot(ine.name.name, true, false)
          case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
        }
        val baseValue = ptaresult.pointsToSet(baseSlot, node.getContext)
        baseValue.foreach{
          ins =>
            result ++= iddg.findDefSite(ins.defSite)
            val arraySlot = ArraySlot(ins)
            val arrayValue = ptaresult.getRelatedInstances(arraySlot, node.getContext)
            arrayValue.foreach(aIns => result ++= iddg.findDefSite(aIns.defSite))
        }
      case ce: CastExp =>
        ce.exp match{
          case ice: NameExp =>
            result ++= searchRda(global, ice.name.name, node, irdaFacts, iddg)
            val slot = VarSlot(ice.name.name, false, false)
            val value = ptaresult.pointsToSet(slot, node.getContext)
            value.foreach{
              ins =>
                val defSite = ins.defSite
                result ++= iddg.findDefSite(defSite)
            }
          case nle: NewListExp => 
          case _ => throw new RuntimeException("Wrong exp: " + ce.exp)
        }
      case ce: CallExp =>
        val calleeSet = if(node.isInstanceOf[IDDGInvokeNode]) node.asInstanceOf[IDDGInvokeNode].getCalleeSet else Set[Callee]()
        ce.arg match {
          case te: TupleExp => 
            val argSlots = te.exps.map{
              exp =>
                exp match{
                  case ne: NameExp => 
                    result ++= searchRda(global, ne.name.name, node, irdaFacts, iddg)
                    VarSlot(ne.name.name, false, true)
                  case _ => VarSlot(exp.toString, false, true)
                }
            }
            calleeSet.foreach{
              callee =>
                val calleeSig = callee.callee
                if(global.isSystemLibraryClasses(calleeSig.getClassType) || global.isUserLibraryClasses(calleeSig.getClassType)) {
                  val sideEffectResult = 
                    if(LibSideEffectProvider.isDefined) LibSideEffectProvider.ipsear.result(calleeSig)
                    else None
                  for(i <- 0 to argSlots.size - 1) {
                    val argSlot = argSlots(i)
                    val argValue = ptaresult.pointsToSet(argSlot, node.getContext)
                    argValue.foreach{ins => result ++= iddg.findDefSite(ins.defSite)}
                    if(sideEffectResult.isDefined) {
                      val readmap = sideEffectResult.get.readMap
                      val writemap = sideEffectResult.get.writeMap
                      val position = i
                      val fields = readmap.getOrElse(position, Set()) ++ writemap.getOrElse(position, Set())
                      argValue.foreach{
                        argIns =>
                          fields.foreach{
                            f => 
                              val fs = FieldSlot(argIns, f)
                              val argRelatedValue = ptaresult.getRelatedInstances(fs, node.getContext)
                              argRelatedValue.foreach{ins => result ++= iddg.findDefSite(ins.defSite)}
                          }
                      }
                    } else if({
                      val calleep = global.getMethod(calleeSig)
                      if(calleep.isDefined) calleep.get.isConcrete
                      else false
                    }) {
                      val argRelatedValue = ptaresult.getRelatedHeapInstances(argValue, node.getContext)
                      argRelatedValue.foreach{ins => result ++= iddg.findDefSite(ins.defSite)}
                    }
                  }
                } else {
                  for(i <- 0 to argSlots.size - 1) {
                    val argSlot = argSlots(i)
                    val argValue = ptaresult.getRelatedInstances(argSlot, node.getContext)
                    argValue.foreach{ins => result ++= iddg.findDefSite(ins.defSite)}
                  }
                }
            }
            result
          case _ => throw new RuntimeException("wrong exp type: " + ce + "  " + ce.arg)
        }
      case _=>
    }
    result
  }

  def processCondition(
      global: Global,
      node: Node, 
      cond: Exp, 
      ptaresult: PTAResult, 
      irdaFacts: ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], 
      iddg: InterProceduralDataDependenceGraph[Node]): ISet[Node] = {
    var result = isetEmpty[Node]
    cond match {
      case be: BinaryExp =>
        result ++= processExp(global, node, be.left, None, ptaresult, irdaFacts, iddg)
        result ++= processExp(global, node, be.right, None, ptaresult, irdaFacts, iddg)
      case _ =>
    }
    result
  }

  def searchRda(
      global: Global,
      varName: String, 
      node: Node, 
      irdaFacts: ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], 
      iddg: InterProceduralDataDependenceGraph[Node]): ISet[Node] = {
    var result: ISet[Node] = isetEmpty
    val varN = varName
    irdaFacts.foreach {
      case ((slot, defDesc), tarContext) => 
        if(varN == slot.toString()) {
          defDesc match {
            case pdd: ParamDefDesc =>
              pdd.locUri match{
                case Some(locU) =>
                  result ++= iddg.findVirtualBodyDefSite(tarContext)
                case None =>
                  throw new RuntimeException("Unexpected ParamDefDesc: " + pdd)
              }
            case ldd: LocDefDesc => 
              ldd.locUri match {
                case Some(locU) =>
                  result ++= iddg.findDefSite(tarContext, true)
                case None =>
                  throw new RuntimeException("Unexpected LocDefDesc: " + ldd)
              }
            case dd: DefDesc =>
              if(dd.isDefinedInitially && !varName.startsWith("@@")){
                val indexs: MSet[Int] = msetEmpty
                val owner = global.getMethod(node.getOwner).get
                var index = 0
                owner.thisOpt foreach{
                  t =>
                    if(t == varName) indexs += index
                    index += 1
                }
                val paramNames = owner.getParamNames
                val paramTyps = owner.getParamTypes
                for(i <- 0 to paramNames.size - 1){
                  val paramName = paramNames(i)
                  if(paramName == varName) indexs += index
                  index += 1
                }
                result ++= indexs.map(i => iddg.findDefSite(tarContext, i))
              }
          }
        }
    }
    result
  }

}
