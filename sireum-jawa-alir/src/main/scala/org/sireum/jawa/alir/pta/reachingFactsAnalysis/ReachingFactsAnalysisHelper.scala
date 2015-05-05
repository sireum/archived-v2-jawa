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
import org.sireum.jawa.JawaMethod
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
import org.sireum.jawa.alir.interProcedural.UnknownCallee
import org.sireum.jawa.PilarAstHelper
import org.sireum.jawa.alir.pta._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object ReachingFactsAnalysisHelper {
  final val TITLE = "ReachingFactsAnalysisHelper"
	def getFactMap(s: ISet[RFAFact]): Map[PTASlot, Set[Instance]] = {
	  s.groupBy(_.s).mapValues(_.map(_.v))
	}
	
	def getHeapFacts(s: ISet[RFAFact]): ISet[(HeapSlot, Instance)] = {
	  s.filter(_.s.isInstanceOf[HeapSlot]).map{f=>(f.s, f.v).asInstanceOf[(HeapSlot, Instance)]}.toSet
	}
	
	def getRelatedFacts(slot: Slot, s: ISet[RFAFact]): ISet[RFAFact] = {
    val bFacts = s.filter(fact=> slot == fact.s)
    val rhFacts = getRelatedHeapFactsFrom(bFacts, s)
    bFacts ++ rhFacts
	}
	
	def getRelatedHeapFactsFrom(fromFacts: ISet[RFAFact], s: ISet[RFAFact]): ISet[RFAFact] = {
	  val insts = fromFacts.map(f => f.v)
	  getRelatedHeapFacts(insts, s)
	}
	
	def getRelatedHeapFacts(insts: ISet[Instance], s: ISet[RFAFact]): ISet[RFAFact] ={
	  val hs = getHeapFacts(s)
    val worklist: MList[Instance] = mlistEmpty ++ insts
    var processed: ISet[Instance] = isetEmpty
    var result: ISet[RFAFact] = isetEmpty
    while(!worklist.isEmpty){
      val ins = worklist.remove(0)
      processed += ins
      val facts = hs.filter(_._1.matchWithInstance(ins)).map{case (k, v) => RFAFact(k, v)}
      result ++= facts
      worklist ++= facts.map{case RFAFact(k, v) => v}.filter{i => !processed.contains(i)}
    }
    result
  }
	
	def getGlobalFacts(s: ISet[RFAFact]): ISet[RFAFact] = {
    var result: ISet[RFAFact] = isetEmpty
    s.foreach{
      fact =>
        fact.s match{
            case vs: VarSlot => 
              if(vs.isGlobal){
                result += fact
                result ++= getRelatedHeapFacts(Set(fact.v), s)
              }
            case bs: BaseSlot =>
              if(bs.isGlobal){
                result += fact
                result ++= getRelatedHeapFacts(Set(fact.v), s)
              }
            case _ =>
          }
    }
    result
  }
	
	def getCalleeSet(cj: CallJump, sig: String, callerContext: Context, ptaresult: PTAResult): ISet[Callee] = {
    val subSig = Center.getSubSigFromMethodSig(sig)
    val typ = cj.getValueAnnotation("type") match {
        case Some(s) => s match {
          case ne: NameExp => ne.name.name
          case _ => ""
        }
        case None => throw new RuntimeException("cannot found annotation 'type' from: " + cj)
      }
    val calleeSet = msetEmpty[Callee]
    if(typ == "virtual" || typ == "interface" || typ == "super" || typ == "direct"){
      cj.callExp.arg match{
        case te: TupleExp => 
          val recvSlot = te.exps(0) match{
            case ne: NameExp => VarSlot(ne.name.name)
            case _ => throw new RuntimeException("wrong exp type: " + te.exps(0))
          }
          val recvValue: ISet[Instance] = ptaresult.pointsToSet(recvSlot, callerContext)
          recvValue.foreach{
			      ins =>
              ins match{
                case ni: NullInstance =>
                  err_msg_normal(TITLE, "Try to invoke method: " + sig + "@" + callerContext + "with Null pointer:" + ins)
                case _ =>
                  if(typ == "super"){
                    val p = CallHandler.getSuperCalleeMethod(sig)
                    calleeSet += InstanceCallee(p, ins)
                  }
                  else if(typ == "direct"){
                    val p = CallHandler.getDirectCalleeMethod(sig)
                    calleeSet += InstanceCallee(p, ins)
                  }
                  else {
                    if(ins.isInstanceOf[UnknownInstance]){
                      val ps = CallHandler.getUnknownVirtualCalleeMethods(ins.typ, subSig)
                      calleeSet ++= ps.map{p=> UnknownCallee(p)}
                    } else {
                      val p = CallHandler.getVirtualCalleeMethod(ins.typ, subSig)
                      calleeSet += InstanceCallee(p, ins)
                    }
                  }
                  
              }
			    }
        case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
      }
    } else {
      val p = CallHandler.getStaticCalleeMethod(sig)
      calleeSet += StaticCallee(p)
    }
    calleeSet.toSet
  }
	
	def getInstanceFromType(typ: Type, currentContext: Context): Option[Instance] = {
	  if(Center.isJavaPrimitiveType(typ) || typ.typ == "void") None
	  else if(typ.typ == "java.lang.String" && !typ.isArray) Some(PTAPointStringInstance(currentContext.copy))
	  else Some(PTAInstance(typ, currentContext.copy))
	}
	  
	def getReturnFact(rType: Type, retVar: String, currentContext: Context): Option[RFAFact] = {
	  val insOpt = getInstanceFromType(rType, currentContext)
	  if(insOpt.isDefined){
	    Some(RFAFact(VarSlot(retVar), insOpt.get))
	  } else None
	}
	
	def getUnknownObject(calleeMethod: JawaMethod, s: PTAResult, args: Seq[String], retVars: Seq[String], currentContext: Context): (ISet[RFAFact], ISet[RFAFact]) = {
	  var genFacts: ISet[RFAFact] = isetEmpty
	  var killFacts: ISet[RFAFact] = isetEmpty
    val argSlots = args.map(arg=>VarSlot(arg))
    for(i <- 0 to argSlots.size - 1){
      val argSlot = argSlots(i)
      val argValues = s.pointsToSet(argSlot, currentContext)
      val influencedFields = if(LibSideEffectProvider.isDefined)
        												LibSideEffectProvider.getInfluencedFields(i, calleeMethod.getSignature)
        										 else Set("ALL")
//      argValues.foreach(_.addFieldsUnknownDefSite(currentContext, influencedFields))
      argValues.foreach{
        ins => 
          influencedFields.foreach{
            f => 
              val fs = FieldSlot(ins, f)
              Center.getField(f) match {
                case Some(field) =>
                  val ins = UnknownInstance(field.getType, currentContext)
                  genFacts += RFAFact(fs, ins)
                case None =>
                  val ins = UnknownInstance(new NormalType(Center.DEFAULT_TOPLEVEL_OBJECT), currentContext)
                  genFacts += RFAFact(fs, ins)
              }
              
          }
      }
    }
//    killFacts ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(argValues, s)
    val retTyp = calleeMethod.getReturnType
    if(!Center.isJavaPrimitiveType(retTyp) && retTyp.name != "void")
	    retVars.foreach{
	      retVar =>
		      val slot = VarSlot(retVar)
	        val value = 
            if(retTyp.name == "java.lang.String") PTAPointStringInstance(currentContext)
            else UnknownInstance(retTyp, currentContext)
	        genFacts += RFAFact(slot, value)
	    }
	  (genFacts, killFacts)
	}
	
	def getUnknownObjectForClinit(calleeMethod: JawaMethod, currentContext: Context): ISet[RFAFact] = {
	  var result: ISet[RFAFact] = isetEmpty
	  val record = calleeMethod.getDeclaringClass
    record.getDeclaredStaticObjectTypeFields.foreach{
      field =>
        result += RFAFact(VarSlot(field.getSignature), UnknownInstance(field.getType, currentContext))
    }
	  result
	}
  
  def updatePTAResultLHSs(lhss: Seq[Exp], currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult) = {
    lhss.foreach{
      key=>
        key match{
          case ne: NameExp =>
          case ae: AccessExp =>
            val fieldSig = ae.attributeName.name
            val baseSlot = ae.exp match {
              case ne: NameExp => BaseSlot(ne.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue = s.filter { fact => fact.s.getId == baseSlot.getId }.map(_.v)
            baseValue.map{
              ins =>
                ptaresult.addInstance(baseSlot, currentContext, ins)
            }
          case ie: IndexingExp =>
            val baseSlot = ie.exp match {
              case ine: NameExp =>
                BaseSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue = s.filter { fact => fact.s.getId == baseSlot.getId }.map(_.v)
            baseValue.map{
              ins =>
                ptaresult.addInstance(baseSlot, currentContext, ins)
            }
          case _=>
        }
    }
  }
  
  private def resolvePTAResultAccessExp(ae: AccessExp, currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult) = {
    val fieldSig = ae.attributeName.name
    val baseSlot = ae.exp match {
      case ne: NameExp => BaseSlot(ne.name.name)
      case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
    }
    val baseValue = s.filter { fact => fact.s.getId == baseSlot.getId }.map{
      f => 
        ptaresult.addInstance(baseSlot, currentContext, f.v)
        f.v
    }
    baseValue.map{
      ins =>
        if(ins.isInstanceOf[NullInstance]){}
        else {
          val recName = StringFormConverter.getClassNameFromFieldSignature(fieldSig)
          val rec = Center.resolveClass(recName, Center.ResolveLevel.HIERARCHY)
          val field = rec.getField(fieldSig)
          val fName = field.getName
          val fieldSlot = FieldSlot(ins, fName)
          s.filter { fact => fact.s == fieldSlot }.map( f => ptaresult.addInstance(fieldSlot, currentContext, f.v))
          s.foreach {
            case fact =>
              if(fact.s.isInstanceOf[FieldSlot] && 
                  fact.s.asInstanceOf[FieldSlot].ins == ins && 
                  fact.s.asInstanceOf[FieldSlot].fieldName.equals("ALL")){
                val uIns = UnknownInstance(field.getType, fact.v.defSite)
                ptaresult.addInstance(fieldSlot, currentContext, uIns)
              }
          }
//          val fieldUnknownValue: MSet[Instance] = msetEmpty
//          ins.getFieldsUnknownDefSites.foreach{
//            case (defsite, fields) =>
//              if(fields.contains("ALL")) fieldUnknownValue += UnknownInstance(field.getType, defsite)
//              else if(fields.contains(fName)) fieldUnknownValue += UnknownInstance(field.getType, defsite)
//          }
//          fieldUnknownValue.map( v => ptaresult.addInstance(fieldSlot, currentContext, v))
        }
    }
  }
  
  private def resolvePTAResultIndexingExp(ie: IndexingExp, currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult) = {
    val baseSlot = ie.exp match {
      case ine: NameExp =>
        BaseSlot(ine.name.name)
      case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
    }
    val baseValue = s.filter { fact => fact.s.getId == baseSlot.getId }.map{
      f => 
        ptaresult.addInstance(baseSlot, currentContext, f.v)
        f.v
    }
    baseValue.map{
      ins =>
        if(ins.isInstanceOf[NullInstance]){}
        else if(ins.isInstanceOf[UnknownInstance]){
          val arraySlot = ArraySlot(ins)
          val temp = s.filter { fact => fact.s == arraySlot }.map{
            f => 
              ptaresult.addInstance(arraySlot, currentContext, f.v)
              f.v
          }
          if(temp.isEmpty){
            val uIns = UnknownInstance(new NormalType(ins.typ.typ, ins.typ.dimensions-1), currentContext)
            ptaresult.addInstance(arraySlot, currentContext, uIns)
          }
        }
        else{
          val arraySlot = ArraySlot(ins)
          s.filter { fact => fact.s == arraySlot }.map( f => ptaresult.addInstance(arraySlot, currentContext, f.v))
        }
    }
  }
  
  def updatePTAResultRHSs(rhss: List[Exp], currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult) = {
    rhss.foreach{
      rhs=>
        updatePTAResultExp(rhs, currentContext, s, ptaresult)
    }
  }
  
  def updatePTAResultCallJump(cj: CallJump, callerContext: Context, s: ISet[RFAFact], ptaresult: PTAResult) = {
    updatePTAResultLHSs(cj.lhss, callerContext, s, ptaresult)
    cj.callExp.arg match{
      case te: TupleExp => 
        te.exps map(updatePTAResultCallArg(_, callerContext, s, ptaresult))
      case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
    }
  }
  
  def updatePTAResultExp(exp: Exp, currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult): Unit = {
    exp match{
      case be: BinaryExp =>
        updatePTAResultExp(be.left, currentContext, s, ptaresult)
        updatePTAResultExp(be.right, currentContext, s, ptaresult)
      case ne: NameExp =>
        val slot = VarSlot(ne.name.name)
        if(slot.isGlobal && StringFormConverter.getFieldNameFromFieldSignature(slot.varName) == "class"){
          val baseName = StringFormConverter.getClassNameFromFieldSignature(slot.varName)
          val rec = Center.resolveClass(baseName, Center.ResolveLevel.HIERARCHY)
          val ins = JawaAlirInfoProvider.getClassInstance(rec)
          ptaresult.addInstance(slot, currentContext, ins)
        } else if(slot.isGlobal){
          Center.findStaticField(ne.name.name) match{
            case Some(af) =>
              val fslot = VarSlot(af.getSignature)
              s.filter { fact => fact.s == fslot }.map( f => ptaresult.addInstance(fslot, currentContext, f.v))
            case None =>
          }
        } else {
          s.filter { fact => fact.s == slot }.map( f => ptaresult.addInstance(slot, currentContext, f.v))
        }
      case ae: AccessExp =>
        resolvePTAResultAccessExp(ae, currentContext, s, ptaresult)
      case ie: IndexingExp =>
        resolvePTAResultIndexingExp(ie, currentContext, s, ptaresult)
      case ce: CastExp =>
        ce.exp match{
          case ice: NameExp =>
            val slot = VarSlot(ice.name.name)
            s.filter { fact => fact.s == slot }.map{
              f => 
                ptaresult.addInstance(slot, currentContext, f.v)
            }
          case nle: NewListExp =>
            System.err.println(TITLE, "NewListExp: " + nle)
          case _ => throw new RuntimeException("Wrong exp: " + ce.exp)
        }
      case _=>
    }
  }
  
  def updatePTAResultCallArg(exp: Exp, currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult): Unit = {
    exp match{
      case ne: NameExp =>
        val slot = VarSlot(ne.name.name)
        getRelatedFacts(slot, s).map( f => ptaresult.addInstance(f.s, currentContext, f.v))
      case _ =>
    }
  }
  
  private def getHeapUnknownFactsExp(exp: Exp, currentContext: Context, ptaresult: PTAResult): ISet[RFAFact] = {
    val result: MSet[RFAFact] = msetEmpty
    exp match {
      case ae: AccessExp =>
//        val fieldSig = ae.attributeName.name
//        val baseSlot = ae.exp match {
//          case ne: NameExp => BaseSlot(ne.name.name)
//          case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
//        }
//        val baseValue = ptaresult.pointsToSet(baseSlot, currentContext)
//        baseValue.map{
//          ins =>
//            if(ins.isInstanceOf[NullInstance]){}
//            else {
//              val recName = StringFormConverter.getClassNameFromFieldSignature(fieldSig)
//              val rec = Center.resolveClass(recName, Center.ResolveLevel.HIERARCHY)
//              val field = rec.getField(fieldSig)
//              val fName = field.getName
//              val fieldSlot = FieldSlot(ins, fName)
//              val fieldUnknownValue: MSet[Instance] = msetEmpty
//              ins.getFieldsUnknownDefSites.foreach{
//                case (defsite, fields) =>
//                  if(fields.contains("ALL")) fieldUnknownValue += UnknownInstance(field.getType, defsite)
//                  else if(fields.contains(fName)) fieldUnknownValue += UnknownInstance(field.getType, defsite)
//              }
//              result ++= fieldUnknownValue.map(ins => RFAFact(fieldSlot, ins))
//            }
//        }
      case ie: IndexingExp =>
        val baseSlot = ie.exp match {
          case ine: NameExp =>
            BaseSlot(ine.name.name)
          case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
        }
        val baseValue = ptaresult.pointsToSet(baseSlot, currentContext)
        baseValue.map{
          ins =>
            if(ins.isInstanceOf[NullInstance]){}
            else if(ins.isInstanceOf[UnknownInstance]){
              val arraySlot = ArraySlot(ins)
              val arrayValue = ptaresult.pointsToSet(arraySlot, currentContext)
              arrayValue.foreach{
                ins =>
                  if(ins.isInstanceOf[UnknownInstance]) result += RFAFact(arraySlot, ins)
              }
            }
        }
      case _ =>
    }
    result.toSet
  }
  
  def getHeapUnknownFacts(rhss: List[Exp], currentContext: Context, ptaresult: PTAResult): ISet[RFAFact] = {
    val result: MSet[RFAFact] = msetEmpty
    rhss.foreach{
      rhs=>
        result ++= getHeapUnknownFactsExp(rhs, currentContext, ptaresult)
    }
    result.toSet
  }
	
	def processLHSs(lhss: List[Exp], currentContext: Context, ptaresult: PTAResult): IMap[Int, IMap[PTASlot, Boolean]] = {
    val result = mmapEmpty[Int, MMap[PTASlot, Boolean]]
    var i = -1
    lhss.foreach{
      key=>
        i += 1
        key match{
          case ne: NameExp =>
            val vs = VarSlot(ne.name.name)
            if(vs.isGlobal){
              Center.findStaticField(ne.name.name) match{
                case Some(af) =>
                  result.getOrElseUpdate(i, mmapEmpty)(VarSlot(af.getSignature)) = true
                case None =>
                  err_msg_normal(TITLE, "Given field may be in other library: " + ne.name.name)
              }
            } else {
              result.getOrElseUpdate(i, mmapEmpty)(vs) = true
            }
          case ae: AccessExp =>
            val fieldSig = ae.attributeName.name
            val baseSlot = ae.exp match {
              case ne: NameExp => BaseSlot(ne.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue = ptaresult.pointsToSet(baseSlot, currentContext)
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[NullInstance])
                  err_msg_normal(TITLE, "Access field: " + baseSlot + "." + fieldSig + "@" + currentContext + "\nwith Null pointer: " + ins)
                else{
                  val recName = StringFormConverter.getClassNameFromFieldSignature(fieldSig)
                  val rec = Center.resolveClass(recName, Center.ResolveLevel.HIERARCHY)
                  val fName = rec.getField(fieldSig).getName
	                if(baseValue.size>1) result.getOrElseUpdate(i, mmapEmpty)(FieldSlot(ins, fName)) = false
	                else result.getOrElseUpdate(i, mmapEmpty)(FieldSlot(ins, fName)) = true
                }
            }
          case ie: IndexingExp =>
            val baseSlot = ie.exp match {
              case ine: NameExp =>
                BaseSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue = ptaresult.pointsToSet(baseSlot, currentContext)
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[NullInstance])
                  err_msg_normal(TITLE, "Access array: " + baseSlot + "@" + currentContext + "\nwith Null pointer: " + ins)
                result.getOrElseUpdate(i, mmapEmpty)(ArraySlot(ins)) = false
            }
          case _=>
        }
    }
    result.map(x => (x._1, x._2.toMap)).toMap
  }
  
//  def checkRHSs(rhss: List[Exp], currentContext: Context, ptaresult: PTAResult): Boolean = {
//    var result = true
//    rhss.foreach{
//      key=>
//        key match{
//          case ae: AccessExp =>
//            val fieldName = ae.attributeName.name
//            val baseSlot = ae.exp match {
//              case ne: NameExp => VarSlot(ne.name.name)
//              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
//            }
//            val baseValue: ISet[Instance] = ptaresult.pointsToSet(baseSlot.toString, currentContext)
//            if(!baseValue.isEmpty) result = false
//          case ie: IndexingExp =>
//            val baseSlot = ie.exp match {
//              case ine: NameExp =>
//                VarSlot(ine.name.name)
//              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
//            }
//            val baseValue: ISet[Instance] = ptaresult.pointsToSet(baseSlot.toString, currentContext)
//            if(!baseValue.isEmpty) result = false
//          case _ => result = false
//        }
//    }
//    result
//  }
  
  def processRHSs(rhss: List[Exp], currentContext: Context, ptaresult: PTAResult): Map[Int, Set[Instance]] = {
    val result = mmapEmpty[Int, Set[Instance]]
    var i = -1
    rhss.foreach{
      rhs=>
        i += 1
        rhs match{
          case ne: NameExp =>
            val slot = VarSlot(ne.name.name)
            var value: ISet[Instance] = isetEmpty
            if(slot.isGlobal && StringFormConverter.getFieldNameFromFieldSignature(slot.varName) == "class"){
              val baseName = StringFormConverter.getClassNameFromFieldSignature(slot.varName)
              val rec = Center.resolveClass(baseName, Center.ResolveLevel.HIERARCHY)
              value += JawaAlirInfoProvider.getClassInstance(rec)
            } else if(slot.isGlobal){
              Center.findStaticField(ne.name.name) match{
                case Some(af) =>
                  value ++= ptaresult.pointsToSet(VarSlot(af.getSignature), currentContext)
                case None =>
                  err_msg_normal(TITLE, "Given field may be in other library: " + ne.name.name)
              }
            } else value ++= ptaresult.pointsToSet(slot, currentContext)
            result(i) = value
          case le: LiteralExp =>
            if(le.typ.name.equals("STRING")){
              val ins = PTAConcreteStringInstance(le.text, currentContext)
              val value: ISet[Instance] = Set(ins)
              result(i) = value
            }
          case ne: NewExp =>
            var name: ResourceUri = ""
            var dimensions = 0
            ne.typeSpec match {
              case nt: NamedTypeSpec => 
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
          case ae: AccessExp =>
            val fieldSig = ae.attributeName.name
            val baseSlot = ae.exp match {
              case ne: NameExp => BaseSlot(ne.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue: ISet[Instance] = ptaresult.pointsToSet(baseSlot, currentContext)
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[NullInstance])
                  err_msg_normal(TITLE, "Access field: " + baseSlot + "." + fieldSig + "@" + currentContext + "\nwith Null pointer: " + ins)
                else {
                  val recName = StringFormConverter.getClassNameFromFieldSignature(fieldSig)
                  val rec = Center.resolveClass(recName, Center.ResolveLevel.HIERARCHY)
                  val field = rec.getField(fieldSig)
                  val fName = field.getName
                  val fieldSlot = FieldSlot(ins, fName)
	                var fieldValue: ISet[Instance] = ptaresult.pointsToSet(fieldSlot, currentContext)
                  if(ins.isInstanceOf[UnknownInstance]){
                    fieldValue += UnknownInstance(field.getType, currentContext)
                  }
			            result(i) = fieldValue
                }
            }
          case ie: IndexingExp =>
            val baseSlot = ie.exp match {
              case ine: NameExp =>
                BaseSlot(ine.name.name)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue: ISet[Instance] = ptaresult.pointsToSet(baseSlot, currentContext)
            baseValue.map{
              ins =>
                if(ins.isInstanceOf[NullInstance])
                  err_msg_normal(TITLE, "Access array: " + baseSlot + "@" + currentContext + " with Null pointer: " + ins)
                else if(ins.isInstanceOf[UnknownInstance]){
                  val arraySlot = ArraySlot(ins)
                  val arrayValue: MSet[Instance] = msetEmpty
                  arrayValue ++= ptaresult.pointsToSet(arraySlot, currentContext)
                  val originalType = ins.typ
                  if(originalType.dimensions == 0) throw new RuntimeException("Some problem must be happened for " + ins + " because indexing cannot happen on 0 dimension object. @" + currentContext)
                  val newType = new NormalType(originalType.typ, originalType.dimensions - 1)
                  val newUnknown = 
                    if(newType.name == "java.lang.String") PTAPointStringInstance(currentContext)
                    else UnknownInstance(newType, currentContext)
                  arrayValue += newUnknown
                  result(i) = arrayValue.toSet
                }
                else {
                  val arraySlot = ArraySlot(ins)
                  val arrayValue: ISet[Instance] = ptaresult.pointsToSet(arraySlot, currentContext)
			            result(i) = arrayValue
                }
            }
          case ce: CastExp =>
            var name: ResourceUri = ""
            var dimensions = 0
            var tmpTs = ce.typeSpec
            while(tmpTs.isInstanceOf[SeqTypeSpec]){
              dimensions += 1
              tmpTs = tmpTs.asInstanceOf[SeqTypeSpec].elementType
            }
            require(tmpTs.isInstanceOf[NamedTypeSpec])
            name = tmpTs.asInstanceOf[NamedTypeSpec].name.name
            val ins = 
              if(name == "java.lang.String" && dimensions == 0){
                PTAPointStringInstance(currentContext.copy)
              } else {
                PTAInstance(new NormalType(name, dimensions), currentContext.copy)
              }
            ce.exp match{
              case ice: NameExp =>
                val slot = VarSlot(ice.name.name)
                val value: ISet[Instance] = ptaresult.pointsToSet(slot, currentContext)
                result(i) = value.map{
                  v =>
                    if(v.isInstanceOf[UnknownInstance]){
                      UnknownInstance(ins.typ, v.defSite.copy)
                    } else {
                      val clazz = Center.getClass(v.typ.name)
                      if(clazz.isChildOf(ins.typ.name)) v
                      else UnknownInstance(ins.typ, v.defSite.copy)
                    }
                }
              case nle: NewListExp =>
                System.err.println(TITLE, "NewListExp: " + nle)
                result(i) = isetEmpty[Instance]// + UnknownInstance(currentContext)
              case _ => throw new RuntimeException("Wrong exp: " + ce.exp)
            }
          case _=>
        }
    }
    result.toMap
  }
  
   def isObjectTypeRegAssignment(a: Assignment): Boolean = {
      var res = false
      a match{
        case aa: AssignAction => 
          a.getValueAnnotation("type") match{
            case Some(e) => 
              e match{
                case ne: NameExp => res = (ne.name.name == "object")
                case _ =>
              }
            case None => 
          }
        case _ =>
      }
      res
  }
  
  def isStaticFieldRead(a: Assignment): Boolean = {
  var result = false
  if(isObjectTypeRegAssignment(a))
  {
    val rhss = PilarAstHelper.getRHSs(a) 
    var i = -1
    rhss.foreach{
      rhs=>
        i += 1
        rhs match{
          case ne: NameExp =>
            val slot = VarSlot(ne.name.name)
            if(slot.isGlobal)
                result = true
          case _ =>
        }
    }    
  }
  result
  }
  
  def isStaticFieldWrite(a: Assignment): Boolean = {
    var result = true
    if(isObjectTypeRegAssignment(a))
    {
     val lhss = PilarAstHelper.getLHSs(a)
     var i = -1
     lhss.foreach{
      key=>
        i += 1
        key match{
          case ne: NameExp =>
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