//package org.sireum.jawa.alir.taintAnalysis
//
//import org.sireum.jawa.alir.controlFlowGraph.InterproceduralControlFlowGraph
//import org.sireum.jawa.alir.controlFlowGraph.CGNode
//import org.sireum.util._
//import org.sireum.jawa.alir.reachingFactsAnalysis.VarSlot
//import org.sireum.jawa.alir.interProcedural._
//import org.sireum.pilar.ast._
//import org.sireum.jawa.alir.controlFlowGraph._
//import org.sireum.jawa.PilarAstHelper
//import org.sireum.jawa.alir.Context
//import org.sireum.jawa.ExceptionCenter
//
//object IMDFTaintAnalysis {
//  final val TITLE = "IMDFTaintAnalysis"
//  
//  def build //
//  (icfg : InterproceduralControlFlowGraph[CGNode],
//   initialFacts : ISet[TaintFact] = isetEmpty,
//   switchAsOrderedMatch : Boolean) : InterProceduralMonotoneDataFlowAnalysisResult[TaintFact] = {
//    val gen = new Gen
//    val kill = new Kill
//    val callr = new Callr
//    val initial : ISet[TaintFact] = isetEmpty
//    val iota : ISet[TaintFact] = initialFacts + TaintFact(VarSlot("@@RFAiota"), new TaintTag)
//    val result = InterProceduralMonotoneDataFlowAnalysisFramework[TaintFact](icfg,
//      true, true, false, false, gen, kill, callr, iota, initial, switchAsOrderedMatch, None)
//    result
//  }
//  
//  class Gen
//      extends InterProceduralMonotonicFunction[TaintFact] {
//    
//    protected def isInterestingAssignment(a : Assignment) : Boolean = {
//      var result = false
//      a match{
//        case aa : AssignAction => 
//          aa.rhs match{
//            case ne : NewExp => result = true
//            case ce : CastExp => result = true
//            case _ =>
//          }
//          a.getValueAnnotation("type") match{
//            case Some(e) => 
//              e match{
//                case ne : NameExp => result = (ne.name.name == "object")
//                case _ =>
//              }
//            case None => 
//          }
//        case cj : CallJump => result = true
//        case _ =>
//      }
//      result
//    }
//    
//    def apply(s : ISet[TaintFact], a : Assignment, currentNode : CGLocNode) : ISet[TaintFact] = {
//      var result : ISet[TaintFact] = isetEmpty
//      if(isInterestingAssignment(a)){
//        val lhss = PilarAstHelper.getLHSs(a)
//        val rhss = PilarAstHelper.getRHSs(a)
//        val slots = ReachingFactsAnalysisHelper.processLHSs(lhss, s, currentNode.getContext)
//        val fieldsFacts = getFieldsFacts(rhss, s, currentNode.getContext)
//        result ++= fieldsFacts
//        val values = ReachingFactsAnalysisHelper.processRHSs(rhss, s , currentNode.getContext) 
//        slots.foreach{
//          case(i, (slot, _)) =>
//            if(values.contains(i))
//              result ++= values(i).map{v => TaintFact(slot, v)}
//        }
//      }
//      result
//    }
//
//    def apply(s : ISet[TaintFact], e : Exp, currentNode : CGLocNode) : ISet[TaintFact] = isetEmpty
//    
//    def apply(s : ISet[TaintFact], a : Action, currentNode : CGLocNode) : ISet[TaintFact] = {
//      var result : ISet[TaintFact] = isetEmpty
//      a match{
//        case ta : ThrowAction =>
//          require(ta.exp.isInstanceOf[NameExp])
//          val slot = VarSlot(ta.exp.asInstanceOf[NameExp].name.name)
//          val value = s.filter(_.s == slot).map(_.v)
//          result ++= value.map(TaintFact(VarSlot(ExceptionCenter.EXCEPTION_VAR_NAME), _))
//        case _ =>
//      }
//      result
//    }
//  }
//
//  class Kill
//      extends InterProceduralMonotonicFunction[TaintFact] {
//    
//    def apply(s : ISet[TaintFact], a : Assignment, currentNode : CGLocNode) : ISet[TaintFact] = {
//      var result = s
//      val lhss = PilarAstHelper.getLHSs(a)
//      val slotsWithMark = ReachingFactsAnalysisHelper.processLHSs(lhss, s, currentNode.getContext).values.toSet
//      val rhss = PilarAstHelper.getRHSs(a)
//      val stop = ReachingFactsAnalysisHelper.checkRHSs(rhss, s)
//      if(stop){
//        result = isetEmpty
//      } else {
//        for (rdf @ TaintFact(slot, _) <- s) {
//          //if it is a strong definition, we can kill the existing definition
//          if (slotsWithMark.contains(slot, true)) {
//            result = result - rdf
//          }
//        }
//      }
//      result
//    }
//
//    def apply(s : ISet[TaintFact], e : Exp, currentNode : CGLocNode) : ISet[TaintFact] = s
//    def apply(s : ISet[TaintFact], a : Action, currentNode : CGLocNode) : ISet[TaintFact] = s
//  }
//  
//  class Callr
//      extends CallResolver[TaintFact] {
//
//    /**
//     * It returns the facts for each callee entry node and caller return node
//     */
//    def resolveCall(s : ISet[TaintFact], cj : CallJump, callerContext : Context, cg : InterproceduralControlFlowGraph[CGNode]) : (IMap[CGNode, ISet[RFAFact]], ISet[RFAFact]) = {
//      val calleeSet = ReachingFactsAnalysisHelper.getCalleeSet(s, cj, callerContext)
//      val cgCallnode = cg.getCGCallNode(callerContext)
//      cgCallnode.asInstanceOf[CGCallNode].setCalleeSet(calleeSet)
//      val cgReturnnode = cg.getCGReturnNode(callerContext)
//      cgReturnnode.asInstanceOf[CGReturnNode].setCalleeSet(calleeSet)
//      var calleeFactsMap : IMap[CGNode, ISet[TaintFact]] = imapEmpty
//      var returnFacts : ISet[TaintFact] = s
//      var tmpReturnFacts : ISet[TaintFact] = isetEmpty
//      var pureNormalFlag = true  //no mix of normal and model callee
//      calleeSet.foreach{
//        callee =>
//          val calleep = callee.callee
//          if(AndroidReachingFactsAnalysisHelper.isICCCall(calleep) || AndroidReachingFactsAnalysisHelper.isModelCall(calleep)){
//            pureNormalFlag = false
//            val args = cj.callExp.arg match{
//              case te : TupleExp =>
//                te.exps.map{
//                  exp =>
//                    exp match{
//                      case ne : NameExp => ne.name.name
//                      case _ => exp.toString()
//                    }
//                }.toList
//              case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
//            }
//            
//            if(AndroidReachingFactsAnalysisHelper.isICCCall(calleep)) {
//              if(AndroidReachingFactsAnalysisConfig.resolve_icc){
//                val factsForCallee = getFactsForICCTarget(s, cj, calleep)
//                returnFacts --= factsForCallee
//                val (retFacts, targets) = AndroidReachingFactsAnalysisHelper.doICCCall(factsForCallee, calleep, args, cj.lhss.map(lhs=>lhs.name.name), callerContext)
//                tmpReturnFacts ++= retFacts
//                targets.foreach{
//                  target =>
//                    if(!cg.isProcessed(target.getSignature, callerContext)){
//                      cg.collectCfgToBaseGraph[String](target, callerContext, false)
//                      cg.extendGraphOneWay(target.getSignature, callerContext, AndroidReachingFactsAnalysis.ICC_EDGE)
//                    }
//                    msg_normal(TITLE, target.getDeclaringRecord + " started!")
//                    calleeFactsMap += (cg.entryNode(target.getSignature, callerContext) -> mapFactsToICCTarget(factsForCallee, cj, target.getProcedureBody.procedure))
//                }
//              }
//            } else { // for non-ICC model call
////              if(callee.getSubSignature == "unknown:()LCenter/Unknown;") println("callees-->" + calleeSet + "\ncontext-->" + callerContext + "\nfacts-->" + s)
//              val factsForCallee = getFactsForCallee(s, cj, calleep)
//              returnFacts --= factsForCallee
//              tmpReturnFacts ++= AndroidReachingFactsAnalysisHelper.doModelCall(factsForCallee, calleep, args, cj.lhss.map(lhs=>lhs.name.name), callerContext)
//            }
//          } else { // for normal call
//            if(!cg.isProcessed(calleep.getSignature, callerContext)){
//              cg.collectCfgToBaseGraph[String](calleep, callerContext, false)
//              cg.extendGraph(calleep.getSignature, callerContext)
//            }
//            val factsForCallee = getFactsForCallee(s, cj, calleep)
//            returnFacts --= factsForCallee
//            calleeFactsMap += (cg.entryNode(calleep.getSignature, callerContext) -> mapFactsToCallee(factsForCallee, cj, calleep.getProcedureBody.procedure))
//          }
//      }
//      returnFacts ++= tmpReturnFacts
//      if(!pureNormalFlag){
//        if(!cg.hasEdge(cgCallnode, cgReturnnode)){
//          cg.addEdge(cgCallnode, cgReturnnode)
//        }
//      }
//      (calleeFactsMap, returnFacts)
//    }
//    
//    private def getFactsForICCTarget(s : ISet[TaintFact], cj : CallJump, callee : JawaProcedure) : ISet[RFAFact] = {
//      val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
//      var calleeFacts = isetEmpty[RFAFact]
//      factMap.foreach{case (slot, v) => 
//        if(slot.isInstanceOf[VarSlot] && slot.asInstanceOf[VarSlot].isGlobal){
//          calleeFacts ++= v.map{r => RFAFact(slot, r)}
//          calleeFacts ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(v, s)
//        }
//      }
//      cj.callExp.arg match{
//        case te : TupleExp => 
//          val exp = te.exps(1) //assume intent always the first arg
//          if(exp.isInstanceOf[NameExp]){
//            val slot = VarSlot(exp.asInstanceOf[NameExp].name.name)
//            var value = factMap.getOrElse(slot, isetEmpty)
//            calleeFacts ++= value.map{r => RFAFact(slot, r)}
//            calleeFacts ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(value, s)
//          }
//          calleeFacts
//        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
//      }
//    }
//    
//    private def getFactsForCallee(s : ISet[RFAFact], cj : CallJump, callee : JawaProcedure) : ISet[RFAFact] = {
//      val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
//      var calleeFacts = isetEmpty[RFAFact]
//      val typ = cj.getValueAnnotation("type") match {
//          case Some(s) => s match {
//            case ne : NameExp => ne.name.name
//            case _ => ""
//          }
//          case None => throw new RuntimeException("cannot found annotation 'type' from: " + cj)
//        }
//      calleeFacts ++= ReachingFactsAnalysisHelper.getGlobalFacts(s)
//      cj.callExp.arg match{
//        case te : TupleExp => 
//          for(i <- 0 to te.exps.size -1){
//            val exp = te.exps(i)
//            if(exp.isInstanceOf[NameExp]){
//              val slot = VarSlot(exp.asInstanceOf[NameExp].name.name)
//              var value = factMap.getOrElse(slot, isetEmpty)
//              if(typ != "static" && i == 0){
//                value = 
//                  value.filter{
//                    r =>
//                      !r.isInstanceOf[NullInstance] && !r.isInstanceOf[UnknownInstance] && shouldPass(r, callee, typ)
//                  }
//              } 
//              calleeFacts ++= value.map{r => RFAFact(slot, r)}
//              calleeFacts ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(value, s)
//            }
//          }
//          calleeFacts
//        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
//      }
//    }
//    
//    /**
//     * return true if the given recv Instance should pass to the given callee
//     */
//    private def shouldPass(recvIns : Instance, calleeProc : JawaProcedure, typ : String) : Boolean = {
//      val recRecv = Center.resolveRecord(recvIns.getType.name, Center.ResolveLevel.HIERARCHY)
//      val recCallee = calleeProc.getDeclaringRecord
//      var tmpRec = recRecv
//      if(typ == "direct" || typ == "super" ){
//        true
//      } else {
//        while(tmpRec.hasSuperClass){
//          if(tmpRec == recCallee) return true
//          else if(tmpRec.declaresProcedure(calleeProc.getSubSignature)) return false
//          else tmpRec = tmpRec.getSuperClass
//        }
//        if(tmpRec == recCallee) return true
//        else {
//          err_msg_detail(TITLE, "Given recvIns: " + recvIns + " and calleeProc: " + calleeProc + " is not in the Same hierachy.")
//          return false
//        }
//      }
//    }
//    
//    def mapFactsToCallee(factsToCallee : ISet[RFAFact], cj : CallJump, calleeProcedure : ProcedureDecl) : ISet[RFAFact] = {
//      val varFacts = factsToCallee.filter(f=>f.s.isInstanceOf[VarSlot] && !f.s.asInstanceOf[VarSlot].isGlobal).map{f=>RFAFact(f.s.asInstanceOf[VarSlot], f.v)}
//      cj.callExp.arg match{
//        case te : TupleExp =>
//          val argSlots = te.exps.map{
//            exp =>
//              exp match{
//                case ne : NameExp => VarSlot(ne.name.name)
//                case _ => VarSlot(exp.toString())
//              }
//          }
//          var paramSlots : List[VarSlot] = List()
//          calleeProcedure.params.foreach{
//            param =>
//              require(param.typeSpec.isDefined)
//              param.typeSpec.get match{
//                case nt : NamedTypeSpec => 
//                  val name = nt.name.name
//                  if(name=="long" || name=="double")
//                    paramSlots :+= VarSlot(param.name.name)
//                case _ =>
//              }
//              paramSlots :+= VarSlot(param.name.name)
//          }
//          var result = isetEmpty[RFAFact]
//          
//          for(i <- 0 to argSlots.size - 1){
//            if(!paramSlots.isDefinedAt(i)){
//              err_msg_critical(TITLE, "argSlots does not adjust to paramSlots:\n" + cj.callExp.arg + "\n" + calleeProcedure.annotations)
//            } else {
//              val argSlot = argSlots(i)
//              val paramSlot = paramSlots(i)
//              varFacts.foreach{
//                fact =>
//                  if(fact.s == argSlot) result += (RFAFact(paramSlot, fact.v))
//              }
//            }
//          }
//          factsToCallee -- varFacts ++ result
//        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
//      }
//    }
//    
//    def mapFactsToICCTarget(factsToCallee : ISet[RFAFact], cj : CallJump, calleeProcedure : ProcedureDecl) : ISet[RFAFact] = {
//      val varFacts = factsToCallee.filter(f=>f.s.isInstanceOf[VarSlot] && !f.s.asInstanceOf[VarSlot].isGlobal).map{f=>RFAFact(f.s.asInstanceOf[VarSlot], f.v)}
//      cj.callExp.arg match{
//        case te : TupleExp =>
//          val argSlot = te.exps(1) match{
//            case ne : NameExp => VarSlot(ne.name.name)
//            case exp => VarSlot(exp.toString())
//          }
//          var paramSlots : List[VarSlot] = List()
//          calleeProcedure.params.foreach{
//            param =>
//              require(param.typeSpec.isDefined)
//              param.typeSpec.get match{
//                case nt : NamedTypeSpec => 
//                  val name = nt.name.name
//                  if(name=="long" || name=="double")
//                    paramSlots :+= VarSlot(param.name.name)
//                case _ =>
//              }
//              paramSlots :+= VarSlot(param.name.name)
//          }
//          var result = isetEmpty[RFAFact]
//          val paramSlot = paramSlots(0)
//          varFacts.foreach{
//            fact =>
//              if(fact.s == argSlot) result += (RFAFact(paramSlot, fact.v))
//          }
//          factsToCallee -- varFacts ++ result
//        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
//      }
//    }
//    
//    private def isReturnJump(loc : LocationDecl) : Boolean = {
//      loc.isInstanceOf[JumpLocation] && loc.asInstanceOf[JumpLocation].jump.isInstanceOf[ReturnJump]
//    }
//    
//    def getAndMapFactsForCaller(calleeS : ISet[RFAFact], callerNode : CGNode, calleeExitNode : CGVirtualNode) : ISet[RFAFact] ={
//      var result = isetEmpty[RFAFact]
//      result ++= ReachingFactsAnalysisHelper.getGlobalFacts(calleeS)
//      callerNode match{
//        case crn : CGReturnNode =>
//          val calleeVarFacts = calleeS.filter(_.s.isInstanceOf[VarSlot]).map{f=>(f.s.asInstanceOf[VarSlot], f.v)}.toSet
//          val calleeProcedure = Center.getProcedureWithoutFailing(calleeExitNode.getOwner).getProcedureBody.procedure
//          val cj = Center.getProcedureWithoutFailing(crn.getOwner).getProcedureBody.location(crn.getLocIndex).asInstanceOf[JumpLocation].jump.asInstanceOf[CallJump]
//          val lhsSlots : ISeq[VarSlot] = cj.lhss.map{lhs=>VarSlot(lhs.name.name)}
//          var paramSlots : List[VarSlot] = List()
//          calleeProcedure.params.foreach{
//            param =>
//              require(param.typeSpec.isDefined)
//              param.typeSpec.get match{
//                case nt : NamedTypeSpec => 
//                  val name = nt.name.name
//                  if(name=="long" || name=="double")
//                    paramSlots :+= VarSlot(param.name.name)
//                case _ =>
//              }
//              paramSlots :+= VarSlot(param.name.name)
//          }
//          val retSlots : MSet[MList[VarSlot]] = msetEmpty
//          calleeProcedure.body match{
//            case ib : ImplementedBody =>
//              ib.locations.foreach{
//                loc=>
//                  if(isReturnJump(loc)){
//                    val rj = loc.asInstanceOf[JumpLocation].jump.asInstanceOf[ReturnJump]
//                    rj.exp match{
//                      case Some(n) => 
//                        n match{
//                          case te : TupleExp => 
//                            val tmplist : MList[VarSlot] = mlistEmpty
//                            te.exps.foreach{
//                              exp =>
//                                exp match {
//                                  case ne : NameExp =>
//                                    tmplist += VarSlot(ne.name.name)
//                                  case _ =>
//                                }
//                            }
//                            retSlots += tmplist
//                          case _ => 
//                        }
//                      case None =>
//                    }
//                  }
//              }
//            case _ =>
//          }
//          lhsSlots.foreach{
//            lhsSlot =>
//              var values : ISet[Instance] = isetEmpty
//              retSlots.foreach{
//                retSlotList =>
//                  calleeVarFacts.foreach{
//                    case (s, v) =>
//                      if(s == retSlotList(lhsSlots.indexOf(lhsSlot))){
//                        values += v
//                      }
//                  }
//              }
//              result ++= values.map(v => RFAFact(lhsSlot, v))
//              result ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(values, calleeS)
//          }
//          cj.callExp.arg match{
//            case te : TupleExp => 
//              val argSlots = te.exps.map{
//                exp =>
//                  exp match{
//                    case ne : NameExp => VarSlot(ne.name.name)
//                    case _ => VarSlot(exp.toString)
//                  }
//              }
//              for(i <- 0 to argSlots.size - 1){
//                val argSlot = argSlots(i)
//                var values : ISet[Instance] = isetEmpty
//                calleeVarFacts.foreach{
//                  case (s, v) =>
//                    if(paramSlots.isDefinedAt(i) && paramSlots(i) == s)
//                      values += v
//                }
//                result ++= values.map(v=>RFAFact(argSlot, v))
//                result ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(values, calleeS)
//              }
//            case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
//          }
//        case cnn : CGNode =>
//      }
//      result
//    }
//  }
//}
//
