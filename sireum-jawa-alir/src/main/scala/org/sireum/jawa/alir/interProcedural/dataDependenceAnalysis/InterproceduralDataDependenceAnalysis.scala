package org.sireum.jawa.alir.interProcedural.dataDependenceAnalysis

import org.sireum.pilar.ast._
import org.sireum.util._
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis._
import org.sireum.jawa.Center
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.alir.NullInstance
import org.sireum.jawa.alir.UnknownInstance
import org.sireum.jawa.JawaProcedure
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.alir.ControlFlowGraph
import org.sireum.alir.ParamDefDesc
import org.sireum.alir.LocDefDesc
import org.sireum.alir.InitDefDesc
import org.sireum.jawa.alir.Context
import org.sireum.alir.DefDesc
import org.sireum.jawa.alir.interProcedural.controlFlowGraph._
import org.sireum.jawa.PilarAstHelper
import org.sireum.alir.AlirEdge
import org.sireum.jawa.alir.interProcedural.InterProceduralMonotoneDataFlowAnalysisResult
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.alir.interProcedural.sideEffectAnalysis.InterProceduralSideEffectAnalysisResult
import org.sireum.jawa.alir.interProcedural.Callee
import org.sireum.jawa.alir.interProcedural.InstanceCallee

trait InterproceduralDataDependenceInfo{
  def getIddg : InterProceduralDataDependenceGraph[InterproceduralDataDependenceAnalysis.Node]
  def getDependentPath(src : InterproceduralDataDependenceAnalysis.Node, dst : InterproceduralDataDependenceAnalysis.Node) : IList[InterproceduralDataDependenceAnalysis.Edge]
  def isDependent(src : InterproceduralDataDependenceAnalysis.Node, dst : InterproceduralDataDependenceAnalysis.Node) : Boolean
}

object InterproceduralDataDependenceAnalysis {
  
  type Node = CGNode
  type Edge = AlirEdge[Node]
  
	def apply(cg : InterproceduralControlFlowGraph[Node], 
	    			rfaResult : InterProceduralMonotoneDataFlowAnalysisResult[RFAFact], 
	    			libSideEffectResultOpt : Option[InterProceduralSideEffectAnalysisResult] = None) : InterproceduralDataDependenceInfo = build(cg, rfaResult, libSideEffectResultOpt)
	
	def build(cg : InterproceduralControlFlowGraph[Node], 
	    			rfaResult : InterProceduralMonotoneDataFlowAnalysisResult[RFAFact], 
	    			libSideEffectResultOpt : Option[InterProceduralSideEffectAnalysisResult]) : InterproceduralDataDependenceInfo = {
    
    class Iddi(iddg : InterProceduralDataDependenceGraph[Node]) extends InterproceduralDataDependenceInfo{
      def getIddg : InterProceduralDataDependenceGraph[InterproceduralDataDependenceAnalysis.Node] = iddg
		  def getDependentPath(src : InterproceduralDataDependenceAnalysis.Node, dst : InterproceduralDataDependenceAnalysis.Node) : IList[InterproceduralDataDependenceAnalysis.Edge] = {
        iddg.findPath(src, dst)
      }
		  def isDependent(src : InterproceduralDataDependenceAnalysis.Node, dst : InterproceduralDataDependenceAnalysis.Node) : Boolean = {
		    getDependentPath(src, dst) != null
		  }
    }
    val irdaResult = InterproceduralReachingDefinitionAnalysis(cg)
	  val iddg = new InterProceduralDataDependenceGraph[Node]
	  iddg.initGraph(cg)
	  iddg.nodes.foreach{
	    node =>
	      var targetNodes : ISet[Node] = isetEmpty
	      if(node != iddg.entryNode && node != iddg.exitNode){
	        node match{
	          case en : CGEntryNode =>
	            val cgN = cg.getCGEntryNode(en.getContext)
	            val cgTarN = cg.predecessors(cgN)
	            targetNodes ++= cgTarN.map(iddg.getNode(_))
	          case en : CGExitNode =>
	          case rn : CGReturnNode =>
	            val tarN = cg.getCGCallNode(rn.getContext)
	            targetNodes += iddg.getNode(tarN)
	          case ln : CGLocNode =>
	            val ownerProc = ln.getOwner
				      val loc = ownerProc.getProcedureBody.location(ln.getLocIndex)
				      val rfaFacts = rfaResult.entrySet(ln)
				      val irdaFacts = irdaResult(ln)
				      targetNodes ++= processLocation(node, loc, rfaFacts, irdaFacts, iddg, libSideEffectResultOpt)
	          case _ =>
	        }
	      }
	      targetNodes.foreach(tn=>iddg.addEdge(node, tn))
	  }
	  
	  msg_normal("[IDDG building done!]")
	  new Iddi(iddg)
	}
	
	def processLocation(node : Node, 
	    						loc : LocationDecl, 
	    						rfaFacts : ISet[RFAFact], 
	    						irdaFacts : ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], 
	    						iddg : InterProceduralDataDependenceGraph[Node], 
	    						libSideEffectResultOpt : Option[InterProceduralSideEffectAnalysisResult]) : ISet[Node] = {
	  var result = isetEmpty[Node]
	  loc match{
		  case al : ActionLocation =>
	      al.action match {
	        case aa : AssignAction =>
	          val lhss = PilarAstHelper.getLHSs(aa)
			      val rhss = PilarAstHelper.getRHSs(aa)
			      result ++= processLHSs(lhss, rfaFacts, irdaFacts, iddg)
			      result ++= processRHSs(node, rhss, rfaFacts, irdaFacts, iddg, libSideEffectResultOpt)
	        case _ =>
	      }
	    case jl : JumpLocation =>
	      jl.jump match{
	        case t : CallJump if t.jump.isEmpty =>
			      val lhss = PilarAstHelper.getLHSs(t)
			      val rhss = PilarAstHelper.getRHSs(t)
			      result ++= processLHSs(lhss, rfaFacts, irdaFacts, iddg)
			      result ++= processRHSs(node, rhss, rfaFacts, irdaFacts, iddg, libSideEffectResultOpt)
			    case gj : GotoJump =>
			    case rj : ReturnJump =>
			      if (rj.exp.isDefined) {
		          processExp(node, rj.exp.get, rfaFacts, irdaFacts, iddg, libSideEffectResultOpt)
		        }
			    case ifj : IfJump =>
			      for (ifThen <- ifj.ifThens) {
              processCondition(node, ifThen.cond, rfaFacts, irdaFacts, iddg, libSideEffectResultOpt)
            }
            if (ifj.ifElse.isEmpty) {
            } else {
            }
			    case sj : SwitchJump =>
			      for (switchCase <- sj.cases) {
              processCondition(node, switchCase.cond, rfaFacts, irdaFacts, iddg, libSideEffectResultOpt)
            }
            if (sj.defaultCase.isEmpty) {
            } else {
            }
	      }
	    case _ =>
	  }
	  result
	}
	
	def processLHSs(lhss : Seq[Exp], rfaFacts : ISet[RFAFact], irdaFacts : ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], iddg : InterProceduralDataDependenceGraph[Node]) : ISet[Node] = {
    var result = isetEmpty[Node]
	  lhss.foreach{
	    lhs =>
	      lhs match{
	        case ne : NameExp =>
          case ae : AccessExp =>
            val baseSlot = ae.exp match {
              case ne : NameExp => 
                result ++= searchRda(ne.name.name, irdaFacts, iddg)
                VarSlot(ne.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue = rfaFacts.filter(f => f.s == baseSlot).map(f => f.v)
            baseValue.foreach{
              ins =>
                val defSite = ins.getDefSite
                result += iddg.findDefSite(defSite)
            }
          case ie : IndexingExp =>
            val baseSlot = ie.exp match {
              case ine : NameExp =>
                result ++= searchRda(ine.name.name, irdaFacts, iddg)
                VarSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue = rfaFacts.filter(f => f.s == baseSlot).map(f => f.v)
            baseValue.foreach{
              ins =>
                val defSite = ins.getDefSite
                result += iddg.findDefSite(defSite)
            }
          case _=>
	      }
	  }
    result
	}
	
	def processRHSs(node : Node, 
	    			rhss : Seq[Exp], 
	    			rfaFacts : ISet[RFAFact], 
	    			irdaFacts : ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], 
	    			iddg : InterProceduralDataDependenceGraph[Node],
	    			libSideEffectResultOpt : Option[InterProceduralSideEffectAnalysisResult]) : ISet[Node] = {
    var result = isetEmpty[Node]
    if(!rhss.isEmpty)
    	result ++= rhss.map(processExp(node, _, rfaFacts, irdaFacts, iddg, libSideEffectResultOpt)).reduce(iunion[Node])
    result
	}
	
	def processExp(node : Node, 
	    		exp : Exp, 
	    		rfaFacts : ISet[RFAFact], 
	    		irdaFacts : ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], 
	    		iddg : InterProceduralDataDependenceGraph[Node],
	    		libSideEffectResultOpt : Option[InterProceduralSideEffectAnalysisResult]) : ISet[Node] = {
	  var result = isetEmpty[Node]
	  exp match{
      case ne : NameExp =>
        result ++= searchRda(ne.name.name, irdaFacts, iddg)
        val slot = VarSlot(ne.name.name)
        val value = rfaFacts.filter(f => f.s == slot).map(f => f.v)
        value.foreach{
          ins =>
            val defSite = ins.getDefSite
            result += iddg.findDefSite(defSite)
        }
      case ae : AccessExp =>
        val fieldSig = ae.attributeName.name
        val baseSlot = ae.exp match {
          case ne : NameExp => 
            result ++= searchRda(ne.name.name, irdaFacts, iddg)
            VarSlot(ne.name.name)
          case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
        }
        val baseValue = rfaFacts.filter(f => f.s == baseSlot).map(f => f.v)
        baseValue.foreach{
          ins =>
            result += iddg.findDefSite(ins.getDefSite)
            if(!ins.isInstanceOf[NullInstance] && !ins.isInstanceOf[UnknownInstance]){
              val recName = StringFormConverter.getRecordNameFromFieldSignature(fieldSig)
              val rec = Center.resolveRecord(recName, Center.ResolveLevel.HIERARCHY)
              if(!rec.isPhantom && !rec.isArray){
	              val fSig = rec.getField(fieldSig).getSignature
		            val fieldSlot = FieldSlot(ins, fSig)
	              val fieldValue = rfaFacts.filter(f => f.s == fieldSlot).map(f => f.v)
	              fieldValue.foreach(fIns => result += iddg.findDefSite(fIns.getDefSite))
              }
            }
        }
      case ie : IndexingExp =>
        val baseSlot = ie.exp match {
          case ine : NameExp =>
            result ++= searchRda(ine.name.name, irdaFacts, iddg)
            VarSlot(ine.name.name)
          case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
        }
        val baseValue = rfaFacts.filter(f => f.s == baseSlot).map(f => f.v)
        baseValue.foreach{
          ins =>
            result += iddg.findDefSite(ins.getDefSite)
            val arraySlot = ArraySlot(ins)
            val arrayValue = ReachingFactsAnalysisHelper.getRelatedFacts(arraySlot, rfaFacts).map(f => f.v)
            arrayValue.foreach(aIns => result += iddg.findDefSite(aIns.getDefSite))
        }
      case ce : CastExp =>
        ce.exp match{
          case ice : NameExp =>
            result ++= searchRda(ice.name.name, irdaFacts, iddg)
            val slot = VarSlot(ice.name.name)
            val value = rfaFacts.filter(f => f.s == slot).map(f => f.v)
            value.foreach{
              ins =>
                val defSite = ins.getDefSite
                result += iddg.findDefSite(defSite)
            }
          case nle : NewListExp => 
          case _ => throw new RuntimeException("Wrong exp: " + ce.exp)
        }
      case ce : CallExp =>
        val calleeSet = if(node.isInstanceOf[CGInvokeNode]) node.asInstanceOf[CGInvokeNode].getCalleeSet else Set[Callee]()
        ce.arg match{
	        case te : TupleExp => 
	          val argSlots = te.exps.map{
	            exp =>
	              exp match{
			            case ne : NameExp => 
			              result ++= searchRda(ne.name.name, irdaFacts, iddg)
			              VarSlot(ne.name.name)
			            case _ => VarSlot(exp.toString)
			          }
	          }
	          calleeSet.foreach{
	            callee =>
	              val calleep = callee.calleeProc
	              if(calleep.getDeclaringRecord.isLibraryRecord){
	                val sideEffectResult = if(libSideEffectResultOpt.isDefined) libSideEffectResultOpt.get.result(calleep.getSignature)
	                											 else None
		              for(i <- 0 to argSlots.size - 1){
				            val argSlot = argSlots(i)
			              val argFacts = 
			                if(callee.isInstanceOf[InstanceCallee] && i == 0) Set(RFAFact(argSlot, callee.asInstanceOf[InstanceCallee].ins))
			                else rfaFacts.filter(fact=> argSlot == fact.s)
			              argFacts.foreach{case RFAFact(slot, ins) => result += iddg.findDefSite(ins.getDefSite)}
			              if(sideEffectResult.isDefined){
			                val readmap = sideEffectResult.get.readMap
			                val writemap = sideEffectResult.get.writeMap
			                val position = calleep.getParamNames.indexOf(argSlot.varName)
			                val fields = readmap.getOrElse(position, Set()) ++ writemap.getOrElse(position, Set())
			                argFacts.foreach{
			                  case RFAFact(slot, argIns) =>
			                    fields.foreach{
			                      f => 
			                        val fs = FieldSlot(argIns, f)
			                        val argRelatedFacts = ReachingFactsAnalysisHelper.getRelatedFacts(fs, rfaFacts)
			                        argRelatedFacts.foreach{case RFAFact(slot, ins) => result += iddg.findDefSite(ins.getDefSite)}
			                    }
			                }
			              } else if(calleep.isConcrete) {
				              val argRelatedFacts = ReachingFactsAnalysisHelper.getRelatedHeapFactsFrom(argFacts, rfaFacts)
                      argRelatedFacts.foreach{case RFAFact(slot, ins) => result += iddg.findDefSite(ins.getDefSite)}
			              }
				          }
	              } else {
	                for(i <- 0 to argSlots.size - 1){
				            val argSlot = argSlots(i)
			              val argFacts = 
			                if(callee.isInstanceOf[InstanceCallee] && i == 0) Set(RFAFact(argSlot, callee.asInstanceOf[InstanceCallee].ins))
			                else rfaFacts.filter(fact=> argSlot == fact.s)
			              argFacts.foreach{case RFAFact(slot, ins) => result += iddg.findDefSite(ins.getDefSite)}
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
	
	def processCondition(node : Node, 
	    			cond : Exp, 
	    			rfaFacts : ISet[RFAFact], 
	    			irdaFacts : ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], 
	    			iddg : InterProceduralDataDependenceGraph[Node],
	    			libSideEffectResultOpt : Option[InterProceduralSideEffectAnalysisResult]) : ISet[Node] = {
	  var result = isetEmpty[Node]
	  cond match{
	    case be : BinaryExp =>
	      result ++= processExp(node, be.left, rfaFacts, irdaFacts, iddg, libSideEffectResultOpt)
	      result ++= processExp(node, be.right, rfaFacts, irdaFacts, iddg, libSideEffectResultOpt)
	    case _ =>
	  }
	  result
	}
	
	def searchRda(varName : String, irdaFacts : ISet[InterproceduralReachingDefinitionAnalysis.IRDFact], iddg : InterProceduralDataDependenceGraph[Node]) : ISet[Node] = {
    var result : ISet[Node] = isetEmpty
    val varN = varName.replaceAll("\\[\\|", "%5B%7C").replaceAll("\\|\\]", "%7C%5D")
    irdaFacts.foreach{
      case ((slot, defDesc), tarContext)=> 
        if(varN == slot.toString()){
          defDesc match {
            case pdd : ParamDefDesc =>
              pdd.locUri match{
                case Some(locU) =>
                  result += iddg.getCGReturnNode(tarContext)
                case None =>
                  throw new RuntimeException("Unexpected ParamDefDesc: " + pdd)
              }
            case ldd : LocDefDesc => 
              ldd.locUri match {
                case Some(locU) =>
                  result += iddg.findDefSite(tarContext)
                case None =>
                  throw new RuntimeException("Unexpected LocDefDesc: " + ldd)
              }
            case dd : DefDesc =>
              if(dd.isDefinedInitially){
	              result += iddg.getCGEntryNode(tarContext)
              }
          }
        }
  	}
    result
  }

}