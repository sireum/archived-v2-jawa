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
package org.sireum.jawa.alir.pta.reachingFactsAnalysis

import org.sireum.util._
import org.sireum.alir.Slot
import org.sireum.jawa.alir.pta.Instance
import org.sireum.pilar.ast._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.JawaMethod
import org.sireum.jawa.alir.JawaAlirInfoProvider
import org.sireum.jawa.alir.util.CallHandler
import org.sireum.jawa.alir.interProcedural.Callee
import org.sireum.jawa.alir.interProcedural.InstanceCallee
import org.sireum.jawa.alir.interProcedural.StaticCallee
import org.sireum.jawa.alir.LibSideEffectProvider
import org.sireum.jawa.alir.interProcedural.UnknownCallee
import org.sireum.jawa.PilarAstHelper
import org.sireum.jawa.alir.pta._
import org.sireum.jawa.Signature
import org.sireum.jawa.JawaType
import org.sireum.jawa.JavaKnowledge
import org.sireum.jawa.util.ASTUtil
import org.sireum.jawa.Global
import org.sireum.jawa.FieldFQN
import org.sireum.jawa.Constants

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object ReachingFactsAnalysisHelper {
  final val TITLE = "ReachingFactsAnalysisHelper"
  def getFactMap(s: ISet[RFAFact])(implicit factory: RFAFactFactory): Map[Int, Set[Int]] = {
    s.groupBy(_.slot).mapValues(_.map(_.ins))
  }

  def getHeapFacts(s: ISet[RFAFact])(implicit factory: RFAFactFactory): ISet[RFAFact] = {
    s.filter(_.s.isInstanceOf[HeapSlot])
  }

  def getRelatedFactsForArg(slot: VarSlot, s: ISet[RFAFact])(implicit factory: RFAFactFactory): ISet[RFAFact] = {
    val bFacts = s.filter(fact=> slot.getId == fact.s.getId).map(fact => RFAFact(factory.getSlotNum(slot), fact.ins))
    val rhFacts = getRelatedHeapFactsFrom(bFacts, s)
    bFacts ++ rhFacts
  }

  def getRelatedHeapFactsFrom(fromFacts: ISet[RFAFact], s: ISet[RFAFact])(implicit factory: RFAFactFactory): ISet[RFAFact] = {
    val insts = fromFacts.map(f => f.ins)
    getRelatedHeapFacts(insts, s)
  }
  
  def getRelatedHeapFacts(insts: ISet[Int], s: ISet[RFAFact])(implicit factory: RFAFactFactory): ISet[RFAFact] = {
    val hs = getHeapFacts(s)
    val worklist: MList[Int] = mlistEmpty ++ insts
    var processed: ISet[Int] = isetEmpty
    var result: ISet[RFAFact] = isetEmpty
    while(!worklist.isEmpty){
      val ins = worklist.remove(0)
      processed += ins
      val facts = hs.filter(_.s.asInstanceOf[HeapSlot].matchWithInstance(factory.getInstance(ins)))
      result ++= facts
      worklist ++= facts.map{case RFAFact(k, v) => v}.filter{i => !processed.contains(i)}
    }
    result
  }

  def getGlobalFacts(s: ISet[RFAFact])(implicit factory: RFAFactFactory): ISet[RFAFact] = {
    var result: ISet[RFAFact] = isetEmpty
    for (fact <- s){
      fact.s match{
        case ss: StaticFieldSlot =>
          result += fact
          result ++= getRelatedHeapFacts(Set(fact.ins), s)
        case _ =>
      }
    }
    result
  }
  
  private def getInstancesOfArg(ce: CallExp, i: Int, callerContext: Context, ptaresult: PTAResult): ISet[Instance] = {
    val value: MSet[Instance] = msetEmpty
    ce.arg match {
      case te: TupleExp =>
        te.exps(i) match{
          case ne: NameExp =>
            val s = VarSlot(ne.name.name, false, true)
            value ++= ptaresult.pointsToSet(s, callerContext)
          case _ =>
        }
      case _ =>
    }
    value.toSet
  }

  def getCalleeSet(global: Global, cj: CallJump, sig: Signature, callerContext: Context, ptaresult: PTAResult): ISet[Callee] = {
    val subSig = sig.getSubSignature
    val typ = cj.getValueAnnotation("kind") match {
      case Some(s) => s match {
        case ne: NameExp => ne.name.name
        case _ => ""
      }
      case None => throw new RuntimeException("cannot found annotation 'kind' from: " + cj)
    }
    val calleeSet = msetEmpty[Callee]
    typ match {
      case "virtual" | "interface" | "super" | "direct" =>
        val recvValue: ISet[Instance] = getInstancesOfArg(cj.callExp, 0, callerContext, ptaresult)
        def handleUnknown(typ: JawaType) = {
//          val ps = CallHandler.getUnknownVirtualCalleeMethods(global, typ, subSig)
          try{
            val c = global.getClassOrResolve(typ)
            calleeSet ++= c.getMethod(subSig).map(m => UnknownCallee(m.getSignature))
          } catch {
            case ie: InterruptedException => throw ie
            case e: Exception =>
          }
        }
        recvValue.foreach{
          ins =>
            if(!ins.isNull)
              if(typ == "super"){
                calleeSet ++= CallHandler.getSuperCalleeMethod(global, sig).map(m => InstanceCallee(m.getSignature, ins))
              } else if(typ == "direct"){
                calleeSet ++= CallHandler.getDirectCalleeMethod(global, sig).map(m => InstanceCallee(m.getSignature, ins))
              } else {
                if(ins.isUnknown){
                  handleUnknown(ins.typ)
                } else {
                  CallHandler.getVirtualCalleeMethod(global, ins.typ, subSig).map(m => InstanceCallee(m.getSignature, ins)) match {
                    case Some(c) => calleeSet += c
                    case None =>
                      handleUnknown(ins.typ)
                  }
                }
              }
        }
        if(recvValue.isEmpty) {
          handleUnknown(sig.getClassType.toUnknown)
        }
      case "static" =>
        calleeSet ++= CallHandler.getStaticCalleeMethod(global, sig).map(m => StaticCallee(m.getSignature))
      case _ => 
    }
    calleeSet.toSet
  }

  def getInstanceFromType(typ: JawaType, currentContext: Context): Option[Instance] = {
    typ match{
      case pt if pt.isPrimitive => None
      case ot if ot.jawaName == "java.lang.String" =>
        Some(PTAPointStringInstance(currentContext.copy))
      case ot if ot.isObject =>
        Some(PTAInstance(ot, currentContext.copy, false))
    }
  }
  
  def getReturnFact(rType: JawaType, retVar: String, currentContext: Context)(implicit factory: RFAFactFactory): Option[RFAFact] = {
    getInstanceFromType(rType, currentContext) map(new RFAFact(VarSlot(retVar, isBase = false, false), _))
  }

  def getUnknownObject(calleeMethod: JawaMethod, s: PTAResult, args: Seq[String], retVars: Seq[String], currentContext: Context)(implicit factory: RFAFactFactory): (ISet[RFAFact], ISet[RFAFact]) = {
    val global = calleeMethod.getDeclaringClass.global
    var genFacts: ISet[RFAFact] = isetEmpty
    var killFacts: ISet[RFAFact] = isetEmpty
    val argSlots = args.map(arg=>VarSlot(arg, isBase = false, true))
    for(i <- 0 to argSlots.size - 1){
      val argSlot = argSlots(i)
      val argValues = s.pointsToSet(argSlot, currentContext)
      val typ: JawaType = 
        if(!calleeMethod.isStatic && i == 0) calleeMethod.getDeclaringClass.typ
        else if(!calleeMethod.isStatic) calleeMethod.getSignature.getParameterTypes()(i - 1)
        else calleeMethod.getSignature.getParameterTypes()(i)
      val influencedFields = 
        if(LibSideEffectProvider.isDefined)
          LibSideEffectProvider.getInfluencedFields(i, calleeMethod.getSignature)
        else Set(typ.name + ":" + Constants.ALL_FIELD)
      argValues.foreach {
        ins => 
          for(f <- influencedFields) {
            val fs = FieldSlot(ins, f)
            val uins = PTAInstance(JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE.toUnknown, currentContext, false)
            genFacts += new RFAFact(fs, uins)
          }
      }
    }
//    killFacts ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(argValues, s)
    val retTyp = calleeMethod.getReturnType
    retTyp match {
      case ot if ot.isObject =>
        retVars.foreach {
          retVar =>
            val slot = VarSlot(retVar, isBase = false, false)
            val value = 
              if(retTyp.jawaName == "java.lang.String") PTAPointStringInstance(currentContext)
              else PTAInstance(ot.toUnknown, currentContext, false)
            genFacts += new RFAFact(slot, value)
        }
      case _ =>
    }
    (genFacts, killFacts)
  }

  def getUnknownObjectForClinit(calleeMethod: JawaMethod, currentContext: Context)(implicit factory: RFAFactFactory): ISet[RFAFact] = {
    var result: ISet[RFAFact] = isetEmpty
    val record = calleeMethod.getDeclaringClass
    record.getDeclaredStaticObjectTypeFields.foreach{
      field =>
        result += new RFAFact(StaticFieldSlot(field.FQN), PTAInstance(field.getType.toUnknown, currentContext, false))
    }
    result
  }
  
  def updatePTAResultLHSs(lhss: Seq[Exp], currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult)(implicit factory: RFAFactFactory) = {
    lhss.foreach{
      key=>
        key match{
          case ne: NameExp =>
          case ae: AccessExp =>
            val fieldSig = ae.attributeName.name
            val baseSlot = ae.exp match {
              case ne: NameExp => VarSlot(ne.name.name, isBase = true, false)
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
                VarSlot(ine.name.name, isBase = true, false)
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
  
  private def resolvePTAResultAccessExp(ae: AccessExp, typ: JawaType, currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult, global: Global)(implicit factory: RFAFactFactory) = {
    val fqn = ASTUtil.getFieldFQN(ae, typ)
    val baseSlot = ae.exp match {
      case ne: NameExp => VarSlot(ne.name.name, isBase = true, false)
      case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
    }
    val baseValue = s.filter { fact => fact.s.getId == baseSlot.getId }.map{
      f => 
        ptaresult.addInstance(baseSlot, currentContext, f.v)
        f.v
    }
    baseValue.map{
      ins =>
        if(ins.isNull){} //TODO show null error message.
        else {
          val fName = fqn.fieldName
          val fieldSlot = FieldSlot(ins, fName)
          s.filter { fact => fact.s == fieldSlot }.map( f => ptaresult.addInstance(fieldSlot, currentContext, f.v))
          s.foreach {
            fact =>
              if(fact.s.isInstanceOf[FieldSlot] && 
                 fact.s.asInstanceOf[FieldSlot].ins == ins && 
                 fact.s.asInstanceOf[FieldSlot].fieldName.contains(Constants.ALL_FIELD) &&
                 fqn.typ.isObject) {
                val definingTypName = fact.s.asInstanceOf[FieldSlot].fieldName.split(":")(0)
                val defCls = global.getClassOrResolve(new JawaType(definingTypName))
                if(defCls.hasField(fName)) {
                  val uIns = PTAInstance(fqn.typ.toUnknown, fact.v.defSite, false)
                  ptaresult.addInstance(fieldSlot, currentContext, uIns)
                }
              }
          }
        }
    }
  }
  
  private def resolvePTAResultIndexingExp(ie: IndexingExp, currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult)(implicit factory: RFAFactFactory) = {
    val baseSlot = ie.exp match {
      case ine: NameExp =>
        VarSlot(ine.name.name, isBase = true, false)
      case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
    }
    val baseValue = s.filter { fact => fact.s.getId == baseSlot.getId }.map{
      f => 
        ptaresult.addInstance(baseSlot, currentContext, f.v)
        f.v
    }
    baseValue.map{
      ins =>
        if(ins.isNull){}
        else if(ins.isUnknown){
          val arraySlot = ArraySlot(ins)
          val temp = s.filter { fact => fact.s == arraySlot }.map{
            f => 
              ptaresult.addInstance(arraySlot, currentContext, f.v)
              f.v
          }
          if(temp.isEmpty){
            if(!(JavaKnowledge.isJavaPrimitive(ins.typ.baseTyp) && ins.typ.dimensions <= 1)) {
              val uIns = PTAInstance(JawaType(ins.typ.baseType, ins.typ.dimensions - 1), currentContext, false)
              ptaresult.addInstance(arraySlot, currentContext, uIns)
            }
          }
        }
        else{
          val arraySlot = ArraySlot(ins)
          s.filter { fact => fact.s == arraySlot }.map( f => ptaresult.addInstance(arraySlot, currentContext, f.v))
        }
    }
  }
  
  def updatePTAResultRHSs(rhss: List[Exp], typ: Option[JawaType], currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult, global: Global)(implicit factory: RFAFactFactory) = {
    rhss.foreach{
      rhs=>
        updatePTAResultExp(rhs, typ, currentContext, s, ptaresult, global)
    }
  }
  
  def updatePTAResultCallJump(cj: CallJump, callerContext: Context, s: ISet[RFAFact], ptaresult: PTAResult)(implicit factory: RFAFactFactory) = {
    cj.callExp.arg match{
      case te: TupleExp => 
        te.exps map(updatePTAResultCallArg(_, callerContext, s, ptaresult))
      case _ => throw new RuntimeException("wrong exp type: " + cj.callExp.arg)
    }
  }
  
  def updatePTAResultExp(exp: Exp, typ: Option[JawaType], currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult, global: Global)(implicit factory: RFAFactFactory): Unit = {
    exp match{
      case be: BinaryExp =>
        updatePTAResultExp(be.left, typ, currentContext, s, ptaresult, global)
        updatePTAResultExp(be.right, typ, currentContext, s, ptaresult, global)
      case ne: NameExp =>
        val slot = getNameSlotFromNameExp(ne, typ, false, false, global)
        slot match {
          case cs: ClassSlot => 
            val ci = ClassInstance(typ.get, currentContext)
            ptaresult.addInstance(cs, currentContext, ci)
          case ss: StaticFieldSlot =>
            s.filter { fact => fact.s == ss }.map( f => ptaresult.addInstance(ss, currentContext, f.v))
          case vs: VarSlot =>
            s.filter { fact => fact.s == vs }.map( f => ptaresult.addInstance(slot, currentContext, f.v))
        }
      case ae: AccessExp =>
        resolvePTAResultAccessExp(ae, typ.get, currentContext, s, ptaresult, global)
      case ie: IndexingExp =>
        resolvePTAResultIndexingExp(ie, currentContext, s, ptaresult)
      case ce: CastExp =>
        ce.exp match{
          case ice: NameExp =>
            val slot = VarSlot(ice.name.name, false, false)
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
  
  def updatePTAResultCallArg(exp: Exp, currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult)(implicit factory: RFAFactFactory): Unit = {
    exp match{
      case ne: NameExp =>
        val slot = VarSlot(ne.name.name, false, true)
        getRelatedFactsForArg(slot, s).map( f => ptaresult.addInstance(f.s, currentContext, f.v))
      case _ =>
    }
  }
  
  private def getHeapUnknownFactsExp(exp: Exp, currentContext: Context, ptaresult: PTAResult)(implicit factory: RFAFactFactory): ISet[RFAFact] = {
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
            VarSlot(ine.name.name, true, false)
          case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
        }
        val baseValue = ptaresult.pointsToSet(baseSlot, currentContext)
        baseValue.map{
          ins =>
            if(ins.isNull){}
            else if(ins.isUnknown){
              val arraySlot = ArraySlot(ins)
              val arrayValue = ptaresult.pointsToSet(arraySlot, currentContext)
              arrayValue.foreach{
                ins =>
                  if(ins.isUnknown) result += new RFAFact(arraySlot, ins)
              }
            }
        }
      case _ =>
    }
    result.toSet
  }
  
  def getHeapUnknownFacts(rhss: List[Exp], currentContext: Context, ptaresult: PTAResult)(implicit factory: RFAFactFactory): ISet[RFAFact] = {
    val result: MSet[RFAFact] = msetEmpty
    rhss.foreach{
      rhs=>
        result ++= getHeapUnknownFactsExp(rhs, currentContext, ptaresult)
    }
    result.toSet
  }

  def processLHSs(lhss: List[Exp], typ: Option[JawaType], currentContext: Context, ptaresult: PTAResult, global: Global): IMap[Int, IMap[PTASlot, Boolean]] = {
    val result = mmapEmpty[Int, MMap[PTASlot, Boolean]]
    var i = -1
    lhss.foreach{
      key=>
        i += 1
        key match{
          case ne: NameExp =>
            val slot = getNameSlotFromNameExp(ne, typ, false, false, global)
            result.getOrElseUpdate(i, mmapEmpty)(slot) = true
          case ae: AccessExp =>
            val fieldFQN = ASTUtil.getFieldFQN(ae, typ.get)
            val baseSlot = ae.exp match {
              case ne: NameExp => VarSlot(ne.name.name, true, false)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue = ptaresult.pointsToSet(baseSlot, currentContext)
            baseValue.map{
              ins =>
                if(ins.isNull) {}
                else{
                  val fName = fieldFQN.fieldName
                  if(baseValue.size>1) result.getOrElseUpdate(i, mmapEmpty)(FieldSlot(ins, fName)) = false
                  else result.getOrElseUpdate(i, mmapEmpty)(FieldSlot(ins, fName)) = true
                }
            }
          case ie: IndexingExp =>
            val baseSlot = ie.exp match {
              case ine: NameExp =>
                VarSlot(ine.name.name, true, false)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue = ptaresult.pointsToSet(baseSlot, currentContext)
            baseValue.map{
              ins =>
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
  
  def processRHSs(rhss: List[Exp], typ: Option[JawaType], currentContext: Context, ptaresult: PTAResult, global: Global): Map[Int, Set[Instance]] = {
    val result = mmapEmpty[Int, Set[Instance]]
    var i = -1
    rhss.foreach{
      rhs=>
        i += 1
        rhs match{
          case ne: NameExp =>
            val slot = getNameSlotFromNameExp(ne, typ, false, false, global)
            var value: ISet[Instance] = ptaresult.pointsToSet(slot, currentContext)
            result(i) = value
          case le: LiteralExp =>
            if(le.typ.name.equals("STRING")){
              val ins = PTAConcreteStringInstance(le.text, currentContext)
              val value: ISet[Instance] = Set(ins)
              result(i) = value
            } else if(le.typ.name.equals("NULL")){
              val inst = if(typ.get.isArray) typ.get else typ.get.toUnknown
              val ins = PTAInstance(inst, currentContext, isNull_ = true)
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
                PTAInstance(new JawaType(name, dimensions), currentContext.copy, false)
              }
            val value: ISet[Instance] = Set(ins)
            result(i) = value
          case ae: AccessExp =>
            val fieldFQN = ASTUtil.getFieldFQN(ae, typ.get)
            val baseSlot = ae.exp match {
              case ne: NameExp => getNameSlotFromNameExp(ne, typ, true, false, global)
              case _ => throw new RuntimeException("Wrong exp: " + ae.exp)
            }
            val baseValue: ISet[Instance] = ptaresult.pointsToSet(baseSlot, currentContext)
            baseValue.map{
              ins =>
                if(ins.isNull){}
                else {
                  val fName = fieldFQN.fieldName
                  val fieldSlot = FieldSlot(ins, fName)
                  var fieldValue: ISet[Instance] = ptaresult.pointsToSet(fieldSlot, currentContext)
                  if(ins.isUnknown && fieldFQN.typ.isObject) {
                    fieldValue += PTAInstance(fieldFQN.typ.toUnknown, currentContext, false)
                  }
                  result(i) = fieldValue
                }
            }
          case ie: IndexingExp =>
            val baseSlot = ie.exp match {
              case ine: NameExp =>
                getNameSlotFromNameExp(ine, typ, true, false, global)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue: ISet[Instance] = ptaresult.pointsToSet(baseSlot, currentContext)
            baseValue.map{
              ins =>
                if(ins.isNull){}
                else if(ins.isUnknown){
                  val arraySlot = ArraySlot(ins)
                  val arrayValue: MSet[Instance] = msetEmpty
                  arrayValue ++= ptaresult.pointsToSet(arraySlot, currentContext)
                  val originalType = ins.typ
                  val dim = if(originalType.dimensions == 0) 0 else originalType.dimensions - 1
                  val newType = JawaType(originalType.baseType, dim)
                  val newUnknown = 
                    if(newType.name == "java.lang.String") PTAPointStringInstance(currentContext)
                    else PTAInstance(newType.toUnknown, currentContext, false)
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
            val casttyp: JawaType = ASTUtil.getTypeFromTypeSpec(ce.typeSpec)
            
            val insopt = 
              if(casttyp.jawaName == "java.lang.String"){
                Some(PTAPointStringInstance(currentContext.copy))
              } else if (casttyp.isObject) {
                Some(PTAInstance(casttyp, currentContext.copy, false))
              } else None
            insopt match {
              case Some(ins) =>
                ce.exp match{
                  case ne: NameExp =>
                    val slot = getNameSlotFromNameExp(ne, typ, false, false, global)
                    val value: ISet[Instance] = ptaresult.pointsToSet(slot, currentContext)
                    result(i) = value.map{
                      v =>
                        if(v.isUnknown){
                          PTAInstance(ins.typ.toUnknown, v.defSite.copy, false)
                        } else {
                          v
                        }
                    }
                  case nle: NewListExp =>
                    System.err.println(TITLE, "NewListExp: " + nle)
                    result(i) = isetEmpty[Instance]// + UnknownInstance(currentContext)
                  case _ => throw new RuntimeException("Wrong exp: " + ce.exp)
                }
              case _ =>
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
        a.getValueAnnotation("kind") match{
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
    if(isObjectTypeRegAssignment(a)) {
      val rhss = PilarAstHelper.getRHSs(a) 
      rhss.foreach{
        rhs=>
          rhs match{
            case ne: NameExp =>
              if(ne.name.name.startsWith("@@")){
                result = true
              }
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
      lhss.foreach{
        key=>
          key match{
            case ne: NameExp =>
              if(ne.name.name.startsWith("@@")){
                result = true
              }
            case _ =>
          }
      }
    }
    result
  }
  
  def getNameSlotFromNameExp(ne: NameExp, typ: Option[JawaType], isBase: Boolean, isArg: Boolean, global: Global): NameSlot = {
    val name = ne.name.name
    if(name == Constants.CONST_CLASS) ClassSlot(typ.get)
    else if(name.startsWith("@@")){
      val fqn = new FieldFQN(name.replace("@@", ""), typ.get)
      global.getClassOrResolve(fqn.owner).getField(fqn.fieldName, fqn.typ) match{
        case Some(af) =>
          StaticFieldSlot(af.FQN)
        case None =>
          StaticFieldSlot(fqn)
      }
    }
    else VarSlot(name, isBase, isArg)
  }
}
