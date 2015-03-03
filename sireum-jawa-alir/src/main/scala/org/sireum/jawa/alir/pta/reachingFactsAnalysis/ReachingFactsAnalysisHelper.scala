/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.pta.reachingFactsAnalysis

import org.sireum.util._
import org.sireum.alir.Slot
import org.sireum.jawa.alir.pta.Instance
import org.sireum.pilar.ast._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.Center
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.alir.pta.NullInstance
import org.sireum.jawa.alir.pta.UnknownInstance
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.NormalType
import org.sireum.jawa.Type
import org.sireum.jawa.alir.JawaAlirInfoProvider
import org.sireum.jawa.alir.util.CallHandler
import org.sireum.jawa.alir.interProcedural.Callee
import org.sireum.jawa.alir.interProcedural.InstanceCallee
import org.sireum.jawa.alir.interProcedural.StaticCallee
import org.sireum.jawa.alir.LibSideEffectProvider
import org.sireum.jawa.alir.pta.UnknownInstance
import org.sireum.jawa.alir.interProcedural.UnknownCallee
import org.sireum.jawa.PilarAstHelper
import org.sireum.jawa.alir.pta.PTAConcreteStringInstance
import org.sireum.jawa.alir.pta.PTAInstance
import org.sireum.jawa.alir.pta.PTAPointStringInstance

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object ReachingFactsAnalysisHelper {
  final val TITLE = "ReachingFactsAnalysisHelper"
	def getFactMap(s : ISet[RFAFact]) : Map[Slot, Set[Instance]] = {
	  s.groupBy(_.s).mapValues(_.map(_.v))
	}
	
	def getHeapFacts(s : ISet[RFAFact]) : ISet[(HeapSlot, Instance)] = {
	  s.filter(_.s.isInstanceOf[HeapSlot]).map{f=>(f.s, f.v).asInstanceOf[(HeapSlot, Instance)]}.toSet
	}
	
	def getRelatedFacts(slot : Slot, s : ISet[RFAFact]) : ISet[RFAFact] = {
    val bFacts = s.filter(fact=> slot == fact.s)
    val rhFacts = getRelatedHeapFactsFrom(bFacts, s)
    bFacts ++ rhFacts
	}
	
	def getRelatedHeapFactsFrom(fromFacts : ISet[RFAFact], s : ISet[RFAFact]) : ISet[RFAFact] = {
	  val insts = fromFacts.map(f => f.v)
	  getRelatedHeapFacts(insts, s)
	}
	
	def getRelatedHeapFacts(insts : ISet[Instance], s : ISet[RFAFact]) : ISet[RFAFact] ={
	  val hs = getHeapFacts(s)
    val worklist : MList[Instance] = mlistEmpty ++ insts
    var processed : ISet[Instance] = isetEmpty
    var result : ISet[RFAFact] = isetEmpty
    while(!worklist.isEmpty){
      val ins = worklist.remove(0)
      processed += ins
      val facts = hs.filter(_._1.matchWithInstance(ins)).map{case (k, v) => RFAFact(k, v)}
      result ++= facts
      worklist ++= facts.map{case RFAFact(k, v) => v}.filter{i => !processed.contains(i)}
    }
    result
  }
	
	def getGlobalFacts(s : ISet[RFAFact]) : ISet[RFAFact] = {
    var result : ISet[RFAFact] = isetEmpty
    s.foreach{
      fact =>
        fact.s match{
            case vs : VarSlot => 
              if(vs.isGlobal){
                result += fact
                result ++= getRelatedHeapFacts(Set(fact.v), s)
              }
            case _ =>
          }
    }
    result
  }
	
	def getCalleeSet(s : ISet[RFAFact], cj : CallJump, callerContext : Context) : ISet[Callee] = {
    val factMap = getFactMap(s)
    val sig = cj.getValueAnnotation("signature") match {
        case Some(s) => s match {
          case ne : NameExp => ne.name.name
          case _ => ""
        }
        case None => throw new RuntimeException("cannot found annotation 'signature' from: " + cj)
      }
    val subSig = Center.getSubSigFromProcSig(sig)
    val typ = cj.getValueAnnotation("type") match {
        case Some(s) => s match {
          case ne : NameExp => ne.name.name
          case _ => ""
        }
        case None => throw new RuntimeException("cannot found annotation 'type' from: " + cj)
      }
    val calleeSet = msetEmpty[Callee]
    if(typ == "virtual" || typ == "interface" || typ == "super" || typ == "direct"){
      cj.callExp.arg match{
        case te : TupleExp => 
          val recvSlot = te.exps(0) match{
            case ne : NameExp => VarSlot(ne.name.name)
            case _ => throw new RuntimeException("wrong exp type: " + te.exps(0))
          }
          val recvValue : ISet[Instance] = factMap.getOrElse(recvSlot, isetEmpty)
          recvValue.foreach{
			      ins =>
              ins match{
                case ni : NullInstance =>
                  err_msg_normal(TITLE, "Try to invoke method: " + sig + "@" + callerContext + "with Null pointer:" + ins)
//                case ui : UnknownInstance =>
//                  err_msg_detail(TITLE, "Invoke method: " + sig + "@" + callerContext + "\n with Unknown Instance: " + ins)
//                  val baseTyp = ui.baseTyp
//                  val baseRecOpt = Center.tryGetRecord(baseTyp.name)
//                  val possibleCalleeSig = Center.getRecordHierarchy.resolveAbstractDispatch(r, p)
//                  calleeSet += UnknownCallee(Center.UNKNOWN_PROCEDURE_SIG, ins)
                case _ =>
                  if(typ == "super"){
                    val p = CallHandler.getSuperCalleeProcedure(sig)
                    calleeSet += InstanceCallee(p, ins)
                  }
                  else if(typ == "direct"){
                    val p = CallHandler.getDirectCalleeProcedure(sig)
                    calleeSet += InstanceCallee(p, ins)
                  }
                  else {
                    if(ins.isInstanceOf[UnknownInstance]){
                      val ps = CallHandler.getUnknownVirtualCalleeProcedures(ins.getType, subSig)
                      calleeSet ++= ps.map{p=> UnknownCallee(p)}
                    } else {
                      val p = CallHandler.getVirtualCalleeProcedure(ins.typ, subSig)
                      calleeSet += InstanceCallee(p, ins)
                    }
                    calleeSet.foreach { p => if(p.callee.isAbstract) println("abstract: " + ins, calleeSet) } 
                  }
                  
              }
			    }
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    } else {
      val p = CallHandler.getStaticCalleeProcedure(sig)
      calleeSet += StaticCallee(p)
    }
    calleeSet.toSet
  }
	
	def getInstanceFromType(typ : Type, currentContext : Context) : Option[Instance] = {
	  if(Center.isJavaPrimitiveType(typ) || typ.typ == "void") None
	  else if(typ.typ == "java.lang.String" && !typ.isArray) Some(PTAPointStringInstance(currentContext))
	  else Some(PTAInstance(typ, currentContext))
	}
	  
	def getReturnFact(rType : Type, retVar : String, currentContext : Context) : Option[RFAFact] = {
	  val insOpt = getInstanceFromType(rType, currentContext)
	  if(insOpt.isDefined){
	    Some(RFAFact(VarSlot(retVar), insOpt.get))
	  } else None
	}
	
	def getUnknownObject(calleeProc : JawaProcedure, s : ISet[RFAFact], args : Seq[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact]) = {
	  var genFacts : ISet[RFAFact] = isetEmpty
	  var killFacts : ISet[RFAFact] = isetEmpty
    val argSlots = args.map(arg=>VarSlot(arg))
    for(i <- 0 to argSlots.size - 1){
      val argSlot = argSlots(i)
      val argValues = s.filter{f=>argSlot == f.s}.map(_.v)
      val influencedFields = if(LibSideEffectProvider.isDefined)
        												LibSideEffectProvider.getInfluencedFields(i, calleeProc.getSignature)
        										 else Set("ALL")
      argValues.foreach(_.addFieldsUnknownDefSite(currentContext, influencedFields))
    }
//    killFacts ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(argValues, s)
    if(!Center.isJavaPrimitiveType(calleeProc.getReturnType))
	    retVars.foreach{
	      retVar =>
		      val slot = VarSlot(retVar)
	        val value = UnknownInstance(calleeProc.getReturnType, currentContext)
	        genFacts += RFAFact(slot, value)
	    }
	  (genFacts, killFacts)
	}
	
	def getUnknownObjectForClinit(calleeProc : JawaProcedure, currentContext : Context) : ISet[RFAFact] = {
	  var result : ISet[RFAFact] = isetEmpty
	  val record = calleeProc.getDeclaringRecord
    record.getDeclaredStaticObjectTypeFields.foreach{
      field =>
        result += RFAFact(VarSlot(field.getSignature), UnknownInstance(field.getType, currentContext))
    }
	  result
	}
	
	def processLHSs(lhss : List[Exp], s : ISet[RFAFact], currentContext : Context) : Map[Int, (Slot, Boolean)] = {
    val factMap = getFactMap(s)
    val result = mmapEmpty[Int, (Slot, Boolean)]
    var i = -1
    lhss.foreach{
      key=>
        i += 1
        key match{
          case ne : NameExp =>
            val vs = VarSlot(ne.name.name)
            if(vs.isGlobal){
              Center.findStaticField(ne.name.name) match{
                case Some(af) =>
                  result(i) = (VarSlot(af.getSignature), true)
                case None =>
                  err_msg_normal(TITLE, "Given field may be in other library: " + ne.name.name)
              }
            } else {
            	result(i) = (vs, true)
            }
          case ae : AccessExp =>
            val fieldSig = ae.attributeName.name
            val baseSlot = ae.exp match {
              case ne : NameExp => VarSlot(ne.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue = factMap.getOrElse(baseSlot, isetEmpty)
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[NullInstance])
                  err_msg_normal(TITLE, "Access field: " + baseSlot + "." + fieldSig + "@" + currentContext + "\nwith Null pointer: " + ins)
                else{
                  val recName = StringFormConverter.getRecordNameFromFieldSignature(fieldSig)
                  val rec = Center.resolveRecord(recName, Center.ResolveLevel.HIERARCHY)
                  val fSig = rec.getField(fieldSig).getSignature
	                if(baseValue.size>1) result(i) = (FieldSlot(ins, fSig), false)
	                else result(i) = (FieldSlot(ins, fSig), true)
                }
            }
          case ie : IndexingExp =>
            val baseSlot = ie.exp match {
              case ine : NameExp =>
                VarSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue = factMap.getOrElse(baseSlot, isetEmpty[Instance])
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[NullInstance])
                  err_msg_normal(TITLE, "Access array: " + baseSlot + "@" + currentContext + "\nwith Null pointer: " + ins)
                result(i) = (ArraySlot(ins), false)
            }
          case _=>
        }
    }
    result.toMap
  }
  
  def checkRHSs(rhss : List[Exp], s : ISet[RFAFact]) : Boolean = {
    val factMap = getFactMap(s)
    var result = true
    rhss.foreach{
      key=>
        key match{
          case ae : AccessExp =>
            val fieldName = ae.attributeName.name
            val baseSlot = ae.exp match {
              case ne : NameExp => VarSlot(ne.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue : ISet[Instance] = factMap.getOrElse(baseSlot, null)
            if(baseValue != null) result = false
          case ie : IndexingExp =>
            val baseSlot = ie.exp match {
              case ine : NameExp =>
                VarSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue : ISet[Instance] = factMap.getOrElse(baseSlot, null)
            if(baseValue != null) result = false
          case _ => result = false
        }
    }
    result
  }
  
  def processRHSs(rhss : List[Exp], s : ISet[RFAFact], currentContext : Context) : Map[Int, Set[Instance]] = {
    val factMap = getFactMap(s)
    val result = mmapEmpty[Int, Set[Instance]]
    var i = -1
    rhss.foreach{
      rhs=>
        i += 1
        rhs match{
          case ne : NameExp =>
            val slot = VarSlot(ne.name.name)
            var value : ISet[Instance] = isetEmpty
            if(slot.isGlobal && StringFormConverter.getFieldNameFromFieldSignature(slot.varName) == "class"){
              val baseName = StringFormConverter.getRecordNameFromFieldSignature(slot.varName)
              val rec = Center.resolveRecord(baseName, Center.ResolveLevel.HIERARCHY)
              value += JawaAlirInfoProvider.getClassInstance(rec)
            } else if(slot.isGlobal){
              Center.findStaticField(ne.name.name) match{
                case Some(af) =>
                  value ++= factMap.getOrElse(VarSlot(af.getSignature), isetEmpty[Instance] + UnknownInstance(af.getType, currentContext))
                case None =>
                  err_msg_normal(TITLE, "Given field may be in other library: " + ne.name.name)
              }
            } else value ++= factMap.getOrElse(slot, isetEmpty[Instance])
            result(i) = value
          case le : LiteralExp =>
            if(le.typ.name.equals("STRING")){
              val ins = PTAConcreteStringInstance(le.text, currentContext)
              val value : ISet[Instance] = Set(ins)
              result(i) = value
            }
          case ne : NewExp =>
            var name : ResourceUri = ""
            var dimensions = 0
            ne.typeSpec match {
              case nt : NamedTypeSpec => 
                dimensions = ne.dims.size + ne.typeFragments.size
                name = nt.name.name
              case _ =>
            }
            
            val ins = 
	            if(name == "java.lang.String" && dimensions == 0){
	              PTAConcreteStringInstance("", currentContext.copy)
	            } else {
	              PTAInstance(new NormalType(name, dimensions), currentContext.copy)
	            }
            var value = isetEmpty[Instance]
            value += ins
            result(i) = value
          case ae : AccessExp =>
            val fieldSig = ae.attributeName.name
            val baseSlot = ae.exp match {
              case ne : NameExp => VarSlot(ne.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue : ISet[Instance] = factMap.getOrElse(baseSlot, isetEmpty[Instance])
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[NullInstance])
                  err_msg_normal(TITLE, "Access field: " + baseSlot + "." + fieldSig + "@" + currentContext + "\nwith Null pointer: " + ins)
                else {
                  val recName = StringFormConverter.getRecordNameFromFieldSignature(fieldSig)
                  val rec = Center.resolveRecord(recName, Center.ResolveLevel.HIERARCHY)
                  val field = rec.getField(fieldSig)
                  val fSig = field.getSignature
                  val fieldSlot = FieldSlot(ins, fSig)
	                val fieldValue : ISet[Instance] = factMap.getOrElse(fieldSlot, isetEmpty)
	                val fieldUnknownValue : MSet[Instance] = msetEmpty
                  ins.getFieldsUnknownDefSites.foreach{
                  	case (defsite, fields) =>
                  	  if(fields.contains("ALL")) fieldUnknownValue += UnknownInstance(field.getType, defsite)
                  	  else if(fields.contains(fSig)) fieldUnknownValue += UnknownInstance(field.getType, defsite)
                	}
			            result(i) = fieldValue ++ fieldUnknownValue
                }
            }
          case ie : IndexingExp =>
            val baseSlot = ie.exp match {
              case ine : NameExp =>
                VarSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue : ISet[Instance] = factMap.getOrElse(baseSlot, isetEmpty[Instance])
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[NullInstance])
                  err_msg_normal(TITLE, "Access array: " + baseSlot + "@" + currentContext + " with Null pointer: " + ins)
                else if(ins.isInstanceOf[UnknownInstance]){
                  val arraySlot = ArraySlot(ins)
                  val arrayValue : ISet[Instance] = factMap.getOrElse(arraySlot, isetEmpty[Instance]) + UnknownInstance(ins.getType, ins.getDefSite)
			            if(arrayValue != null)
			            	result(i) = arrayValue
                }
                else{
                  val arraySlot = ArraySlot(ins)
                  val arrayValue : ISet[Instance] = factMap.getOrElse(arraySlot, isetEmpty[Instance])
			            if(arrayValue != null)
			            	result(i) = arrayValue
                }
            }
          case ce : CastExp =>
            var name : ResourceUri = ""
            var dimensions = 0
            ce.typeSpec match {
              case nt : NamedTypeSpec => 
                name = nt.name.name
              case _ =>
            }
            
            val ins = 
              if(name == "java.lang.String" && dimensions == 0){
                PTAConcreteStringInstance("", currentContext.copy)
              } else {
                PTAInstance(new NormalType(name, dimensions), currentContext.copy)
              }
            ce.exp match{
              case ice : NameExp =>
                val slot = VarSlot(ice.name.name)
                val value : ISet[Instance] = factMap.getOrElse(slot, isetEmpty[Instance])
                result(i) = value.map{
                  v =>
                    if(v.isInstanceOf[UnknownInstance]){
                      UnknownInstance(ins.getType, v.defSite)
                    } else v
                }
              case nle : NewListExp =>
                System.err.println(TITLE, "NewListExp: " + nle)
                result(i) = isetEmpty[Instance]// + UnknownInstance(currentContext)
              case _ => throw new RuntimeException("Wrong exp: " + ce.exp)
            }
          case _=>
        }
    }
    result.toMap
  }
  
   def isObjectTypeRegAssignment(a : Assignment): Boolean = {
      var res = false
      a match{
        case aa : AssignAction => 
          a.getValueAnnotation("type") match{
            case Some(e) => 
              e match{
                case ne : NameExp => res = (ne.name.name == "object")
                case _ =>
              }
            case None => 
          }
        case _ =>
      }
      res
  }
  
  def isStaticFieldRead(a : Assignment) : Boolean = {
  var result = false
  if(isObjectTypeRegAssignment(a))
  {
    val rhss = PilarAstHelper.getRHSs(a) 
    var i = -1
    rhss.foreach{
      rhs=>
        i += 1
        rhs match{
          case ne : NameExp =>
            val slot = VarSlot(ne.name.name)
            if(slot.isGlobal)
                result = true
          case _ =>
        }
    }    
  }
  result
  }
  
  def isStaticFieldWrite(a : Assignment) : Boolean = {
    var result = true
    if(isObjectTypeRegAssignment(a))
    {
     val lhss = PilarAstHelper.getLHSs(a)
     var i = -1
     lhss.foreach{
      key=>
        i += 1
        key match{
          case ne : NameExp =>
            val vs = VarSlot(ne.name.name)
            if(vs.isGlobal){
              result = true
            }
          case _ =>
        }
      }
    }
   result
   }
  
}