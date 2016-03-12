/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa.alir.sideEffectAnalysis

import org.sireum.jawa._
import org.sireum.jawa.alir.JawaAlirInfoProvider
import org.sireum.pilar.ast._
import org.sireum.jawa.alir.reachingDefinitionAnalysis.JawaReachingDefinitionAnalysis
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.LocDefDesc
import org.sireum.jawa.alir.util.CallHandler
import org.sireum.util._
import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.immutable.ParSet
import scala.collection.GenMap

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait InterProceduralSideEffectAnalysisResult {
  def result : Signature => Option[InterProceduralSideEffectResult]
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
case class InterProceduralSideEffectResult(procSig : Signature,
                                           readMap : Map[Int, Set[String]],
                                           writeMap : Map[Int, Set[String]],
                                           globalRead : Set[String],
                                           globalWrite : Set[String]) {
  override def toString : String = {
    val sb : StringBuilder = new StringBuilder()
    sb.append("Method:" + procSig + "\n")
    sb.append("readMap:\n")
    readMap.foreach{
      case (i, fields) => sb.append(i + "," + fields + "\n")
    }
    sb.append("writeMap:\n")
    writeMap.foreach{
      case (i, fields) => sb.append(i + "," + fields + "\n")
    }
    sb.append("globalRead:\n")
    globalRead.foreach{
      gl => sb.append(gl + "\n")
    }
    sb.append("globalWrite:\n")
    globalWrite.foreach{
      gl => sb.append(gl + "\n")
    }
    sb.toString().intern
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
case class IntraProceduralSideEffectResult(
    procedure : JawaMethod,
    readMap : Map[Int, Set[String]],
    writeMap : Map[Int, Set[String]],
    globalRead : Set[String],
    globalWrite : Set[String],
    callInfos : Set[CallInfo]) {
  override def toString : String = {
    val sb : StringBuilder = new StringBuilder()
    sb.append("Method:" + procedure + "\n")
    sb.append("readMap:\n")
    readMap.foreach{
      case (i, fields) => sb.append(i + ":" + fields + "\n")
    }
    sb.append("writeMap:\n")
    writeMap.foreach{
      case (i, fields) => sb.append(i + ":" + fields + "\n")
    }
    sb.append("globalRead:\n")
    globalRead.foreach{
      gl => sb.append(gl + "\n")
    }
    sb.append("globalWrite:\n")
    globalWrite.foreach{
      gl => sb.append(gl + "\n")
    }
    callInfos.foreach{
      ci =>
        sb.append(ci.toString + "\n")
    }
    sb.toString().intern
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
case class CallInfo(callees : Set[JawaMethod], paramMap : Map[Int, Int]){
  override def toString : String = {
    val sb : StringBuilder = new StringBuilder()
    sb.append("CallInfo(" + callees + ",")
    paramMap.foreach{
      case (argP, paramP) => sb.append("[" + argP + "," + paramP + "]")
    }
    sb.append(")")
    sb.toString().intern
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object SideEffectAnalysis {
  
  def interProceduralSideEffect(intraSEResults : GenMap[Signature, IntraProceduralSideEffectResult]) : InterProceduralSideEffectAnalysisResult = {
    val results : MMap[Signature, InterProceduralSideEffectResult] = mmapEmpty
    def getResult(sig : Signature) : Option[InterProceduralSideEffectResult] = results.get(sig)
    class Ipsea(val result : Signature => Option[InterProceduralSideEffectResult]) extends InterProceduralSideEffectAnalysisResult {
      
      override def toString : String = {
        val sb = new StringBuilder

        for (n <- results) {
          sb.append("%s\n".format(getResult(n._1)))
        }
        sb.append("\n")

        sb.toString
      }
      
      override def equals(cr : Any) : Boolean = {
        if(cr.isInstanceOf[InterProceduralSideEffectAnalysisResult])
          results.forall{case (sig, r) => (r.toString == cr.asInstanceOf[InterProceduralSideEffectAnalysisResult].result(sig).get.toString)}
        else false
      }
      
      def add(ipser : InterProceduralSideEffectResult) : Unit = {
        this.synchronized(
          results.update(ipser.procSig, ipser)
        )
      }
    }
    
    val ipsea = new Ipsea(getResult _)
    var x : Float = 0
    val procSize = intraSEResults.size
    val rs =
      intraSEResults.par.map{
        case (procSig, intraPSE) =>
          this.synchronized(x += 1)
          if(x%1000==0)println((x/procSize)*100 + "%")
          if(x == procSize) println("Inter side effect Done!")
          resolveInterProceduralSideEffect(intraPSE, intraSEResults)
      }
    rs.foreach(ipsea.add(_))
    ipsea
  }
  
  private def resolveInterProceduralSideEffect(intraPSE : IntraProceduralSideEffectResult, intraSEResults : GenMap[Signature, IntraProceduralSideEffectResult]) : InterProceduralSideEffectResult = {
    var worklist : Set[CallInfo] = Set()
    val processed : MSet[CallInfo] = msetEmpty
    worklist ++= intraPSE.callInfos
    val procedure = intraPSE.procedure
    var readMap : Map[Int, Set[String]] = intraPSE.readMap
    var writeMap : Map[Int, Set[String]] = intraPSE.writeMap
    var globalRead : Set[String] = intraPSE.globalRead
    var globalWrite : Set[String] = intraPSE.globalWrite
    while(!worklist.isEmpty){
      processed ++= worklist
      worklist = worklist.par.map{
        call =>
          var newCalls = Set[CallInfo]()
          val paramMap = call.paramMap
          call.callees.foreach{
            callee =>
              val calleeOpt = intraSEResults.get(callee.getSignature)
              calleeOpt match{
                case Some (calleeIse) =>
                  paramMap.foreach{
                    case (argP, paramP) =>
                      val reads = calleeIse.readMap.getOrElse(argP, Set())
                      readMap += (paramP -> (readMap.getOrElse(paramP, Set()) ++ reads))
                      val writes = calleeIse.writeMap.getOrElse(argP, Set())
                      writeMap += (paramP -> (writeMap.getOrElse(paramP, Set()) ++ writes))
                      globalRead ++= calleeIse.globalRead
                      globalWrite ++= calleeIse.globalWrite
                      newCalls = calleeIse.callInfos -- processed
                  }
                case None =>
              }
          }
          newCalls
      }.reduce(iunion[CallInfo])
    }
    InterProceduralSideEffectResult(procedure.getSignature, readMap, writeMap, globalRead, globalWrite)
  }
  
  def intraProceduralSideEffect(global: Global, procedure : JawaMethod) : IntraProceduralSideEffectResult = {
    var readMap : Map[Int, Set[String]] = Map()
    var writeMap : Map[Int, Set[String]] = Map()
    var globalRead : Set[String] = Set()
    var globalWrite : Set[String] = Set()
    var callInfos : Set[CallInfo] = Set()
    val cfg = JawaAlirInfoProvider.getCfg(procedure)
    val rda = JawaAlirInfoProvider.getRda(procedure, cfg)
    val points = new PointsCollector().points(procedure.getSignature, procedure.getBody)
    points.foreach{
      point =>
        point match{
          case pa : PointAsmt =>
            pa.lhs match{
              case pfl : PointFieldL =>
                val varName = pfl.baseP.baseName
                val fieldName = pfl.fqn.fieldName
                val position = findPositionFromRda(procedure, cfg, rda, varName, Some(pfl.loc), pfl.locIndex)
                if(position >= 0)
                writeMap += (position -> (writeMap.getOrElse(position, Set()) + fieldName))
              case pgl : PointStaticFieldL =>
                val globalSig = pgl.staticFieldFQN.toString()
                globalWrite += globalSig
              case _ =>
            }
            pa.rhs match{
              case pfr : PointFieldR =>
                val varName = pfr.baseP.baseName
                val fieldName = pfr.fqn.fieldName
                val position = findPositionFromRda(procedure, cfg, rda, varName, Some(pfr.loc), pfr.locIndex)
                if(position >= 0)
                readMap += (position -> (readMap.getOrElse(position, Set()) + fieldName))
              case pgr : PointStaticFieldR =>
                val globalSig = pgr.staticFieldFQN.toString()
                globalRead += globalSig
              case _ =>
            }
          case pi : Point with Loc with Invoke =>
            var paramMap : Map[Int, Int] = Map()
            val callSig = pi.sig
            val callTyp = pi.invokeTyp
            pi match{
              case p : PointI => 
                val varName = p.recvPCall.argName
                val position = findPositionFromRda(procedure, cfg, rda, varName, Some(p.loc), p.locIndex)
                if(position >= 0)
                  paramMap += (0 -> position)
              case _ => 
            }
            pi.argPsCall.foreach{
              case (i, arg) =>
                val varName = arg.argName
                val position = findPositionFromRda(procedure, cfg, rda, varName, Some(pi.loc), pi.locIndex)
                val argPosition = arg.index
                if(position >= 0)
                  paramMap += (argPosition -> position)
            }
            val callees = CallHandler.resolveSignatureBasedCall(global, callSig, callTyp)
            callInfos += CallInfo(callees, paramMap)
          case _ =>
        }
    }
    IntraProceduralSideEffectResult(procedure, readMap, writeMap, globalRead, globalWrite, callInfos)
  }
  
  private def findPositionFromRda(
      procedure : JawaMethod, 
      cfg : ControlFlowGraph[String], 
      rda : JawaReachingDefinitionAnalysis.Result, 
      varName : String, 
      locUri : Option[String], 
      locIndex : Int,
      slotStack : Set[JawaReachingDefinitionAnalysis.RDFact] = Set()) : Int = {
    val paramNameList = procedure.getParamNames
    val slots = rda.entrySet(cfg.getNode(locUri, locIndex)) -- slotStack
    slots.foreach{
      case (slot, defDesc)=> 
        if(varName == slot.toString){
          if(defDesc.isDefinedInitially){
            if(!varName.startsWith("@@")){
              return paramNameList.indexOf(varName)
            } else {
              return -1  // postion for global variable
            }
          } else if(defDesc.isUndefined) {
            return -2  // postion for local variable
          } else if(defDesc.isInstanceOf[LocDefDesc]) {
            val locDefDesc = defDesc.asInstanceOf[LocDefDesc]
            val loc = procedure.getBody.location(locDefDesc.locIndex)
            if(isSimpleAssignment(loc)){
              val rhsVar = getAssignmentRhsVar(loc)
              return findPositionFromRda(procedure, cfg, rda, rhsVar, locDefDesc.locUri, locDefDesc.locIndex, slotStack ++ slots)
            } else {
              return -2
            }
          } else throw new RuntimeException("Unexpected defdesc type: " + defDesc)
        }
  }
    -2
  }
  
  private def isSimpleAssignment(loc : LocationDecl) : Boolean = {
    loc match{
      case al : ActionLocation =>
        al.action match{
          case aa : AssignAction =>
            aa.lhs.isInstanceOf[NameExp] && aa.rhs.isInstanceOf[NameExp]
          case _ => false
        }
      case _ => false
    }
  }
  
  private def getAssignmentRhsVar(loc : LocationDecl) : String = {
    loc match{
      case al : ActionLocation =>
        al.action match{
          case aa : AssignAction =>
            require(aa.rhs.isInstanceOf[NameExp])
            aa.rhs.asInstanceOf[NameExp].name.name
          case _ => throw new RuntimeException
        }
      case _ => throw new RuntimeException
    }
  }
}
