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

trait InterProceduralSideEffectAnalysisResult {
  def result : String => Option[InterProceduralSideEffectResult]
}

case class InterProceduralSideEffectResult(procSig : String,
																							readMap : Map[Int, Set[String]],
																							writeMap : Map[Int, Set[String]],
																							globalRead : Set[String],
  																						globalWrite : Set[String]) {
  override def toString : String = {
    val sb : StringBuilder = new StringBuilder()
    sb.append("Procedure:" + procSig + "\n")
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

case class IntraProceduralSideEffectResult(procedure : JawaProcedure,
																						readMap : Map[Int, Set[String]],
																						writeMap : Map[Int, Set[String]],
																						globalRead : Set[String],
																						globalWrite : Set[String],
																						callInfos : Set[CallInfo]) {
  override def toString : String = {
    val sb : StringBuilder = new StringBuilder()
    sb.append("Procedure:" + procedure + "\n")
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

case class CallInfo(callees : Set[JawaProcedure], paramMap : Map[Int, Int]){
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

object SideEffectAnalysis {	
  
  def interProceduralSideEffect(intraSEResults : ParMap[String, IntraProceduralSideEffectResult]) : InterProceduralSideEffectAnalysisResult = {
    val results : MMap[String, InterProceduralSideEffectResult] = mmapEmpty
    def getResult(sig : String) : Option[InterProceduralSideEffectResult] = results.get(sig)
    class Ipsea(val result : String => Option[InterProceduralSideEffectResult]) extends InterProceduralSideEffectAnalysisResult {
      
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
  
  private def resolveInterProceduralSideEffect(intraPSE : IntraProceduralSideEffectResult, intraSEResults : ParMap[String, IntraProceduralSideEffectResult]) : InterProceduralSideEffectResult = {
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
  																						
	def intraProceduralSideEffect(procedure : JawaProcedure) : IntraProceduralSideEffectResult = {
    var readMap : Map[Int, Set[String]] = Map()
    var writeMap : Map[Int, Set[String]] = Map()
    var globalRead : Set[String] = Set()
    var globalWrite : Set[String] = Set()
    var callInfos : Set[CallInfo] = Set()
    val cfg = JawaAlirInfoProvider.getCfg(procedure)
    val rda = JawaAlirInfoProvider.getRda(procedure, cfg)
    val points = new PointsCollector().points(procedure.getSignature, procedure.getProcedureBody)
    points.foreach{
      point =>
        point match{
          case pa : PointAsmt =>
            pa.lhs match{
              case pfl : PointFieldL =>
	              val varName = pfl.basePoint.varName
		            val fieldName = pfl.getFieldName
		            val position = findPositionFromRda(procedure, cfg, rda, varName, Some(pfl.locationUri), pfl.locationIndex)
		            if(position >= 0)
		            	writeMap += (position -> (writeMap.getOrElse(position, Set()) + fieldName))
              case pgl : PointGlobalL =>
                val globalName = pgl.varName
                globalWrite += globalName
              case _ =>
            }
            pa.rhs match{
              case pfr : PointFieldR =>
		            val varName = pfr.basePoint.varName
		            val fieldName = pfr.getFieldName
		            val position = findPositionFromRda(procedure, cfg, rda, varName, Some(pfr.locationUri), pfr.locationIndex)
		            if(position >= 0)
		            	readMap += (position -> (readMap.getOrElse(position, Set()) + fieldName))
              case pgr : PointGlobalR =>
                val globalName = pgr.varName
                globalRead += globalName
              case _ =>
            }
          case pi : PointI =>
            var paramMap : Map[Int, Int] = Map()
            val callSig = pi.varName
            val callTyp = pi.typ
            var hasRecv = true
            pi.recvOpt_Call match{
              case Some(recv) => 
                val varName = recv.varName
                val position = findPositionFromRda(procedure, cfg, rda, varName, Some(pi.locationUri), pi.locationIndex)
                if(position >= 0)
                	paramMap += (0 -> position)
              case None => hasRecv = false
            }
            pi.args_Call.foreach{
              case (i, arg) =>
                val varName = arg.varName
                val position = findPositionFromRda(procedure, cfg, rda, varName, Some(pi.locationUri), pi.locationIndex)
                val argPosition = if(hasRecv) i + 1 else i
                if(position >= 0)
                	paramMap += (argPosition -> position)
            }
            val callees = CallHandler.resolveSignatureBasedCall(callSig, callTyp)
            callInfos += CallInfo(callees, paramMap)
          case _ =>
        }
    }
	  IntraProceduralSideEffectResult(procedure, readMap, writeMap, globalRead, globalWrite, callInfos)
	}
  
  private def findPositionFromRda(procedure : JawaProcedure, 
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
              return -1			// postion for global variable
            }
          } else if(defDesc.isUndefined) {
            return -2				// postion for local variable
          } else if(defDesc.isInstanceOf[LocDefDesc]) {
            val locDefDesc = defDesc.asInstanceOf[LocDefDesc]
            val loc = procedure.getProcedureBody.location(locDefDesc.locIndex)
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