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
import org.sireum.jawa.alir.pta.UnknownInstance
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
import org.sireum.jawa.ObjectType
import org.sireum.jawa.JawaType
import org.sireum.jawa.JavaKnowledge
import org.sireum.jawa.PrimitiveType
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
    for (fact <- s){
      fact.s match{
        case ss: StaticFieldSlot =>
          result += fact
          result ++= getRelatedHeapFacts(Set(fact.v), s)
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
            val s = VarSlot(ne.name.name, false)
            value ++= ptaresult.pointsToSet(s, callerContext)
          case _ =>
        }
      case _ =>
    }
    value.toSet
  }

  def getCalleeSet(global: Global, cj: CallJump, sig: Signature, callerContext: Context, ptaresult: PTAResult): ISet[Callee] = {
    val subSig = sig.getSubSignature
    val typ = cj.getValueAnnotation("type") match {
      case Some(s) => s match {
        case ne: NameExp => ne.name.name
        case _ => ""
      }
      case None => throw new RuntimeException("cannot found annotation 'type' from: " + cj)
    }
    val calleeSet = msetEmpty[Callee]
    typ match {
      case "virtual" | "interface" | "super" | "direct" =>
        val recvValue: ISet[Instance] = getInstancesOfArg(cj.callExp, 0, callerContext, ptaresult)
        recvValue.foreach{
          ins =>
            if(typ == "super"){
              calleeSet ++= CallHandler.getSuperCalleeMethod(global, sig).map(InstanceCallee(_, ins))
            } else if(typ == "direct"){
              calleeSet ++= CallHandler.getDirectCalleeMethod(global, sig).map(InstanceCallee(_, ins))
            } else {
              if(ins.isInstanceOf[UnknownInstance]){
                val ps = CallHandler.getUnknownVirtualCalleeMethods(global, ins.typ, subSig)
                calleeSet ++= ps.map{p=> UnknownCallee(p)}
              } else {
                calleeSet ++= CallHandler.getVirtualCalleeMethod(global, ins.typ, subSig).map(InstanceCallee(_, ins))
              }
            }
        }
      case "static" => 
        calleeSet ++= CallHandler.getStaticCalleeMethod(global, sig).map(StaticCallee(_))
      case _ => 
    }
    calleeSet.toSet
  }

  def getInstanceFromType(typ: JawaType, currentContext: Context): Option[Instance] = {
    typ match{
      case pt: PrimitiveType => None
      case ot: ObjectType if (ot.typ == "java.lang.String" && !ot.isArray)  =>
        Some(PTAPointStringInstance(currentContext.copy))
      case ot: ObjectType =>
        Some(PTAInstance(ot, currentContext.copy, false))
    }
  }
  
  def getReturnFact(rType: JawaType, retVar: String, currentContext: Context): Option[RFAFact] = {
    getInstanceFromType(rType, currentContext) map(RFAFact(VarSlot(retVar, isBase = false), _))
  }

  def getUnknownObject(calleeMethod: JawaMethod, s: PTAResult, args: Seq[String], retVars: Seq[String], currentContext: Context): (ISet[RFAFact], ISet[RFAFact]) = {
    val global = calleeMethod.getDeclaringClass.global
    var genFacts: ISet[RFAFact] = isetEmpty
    var killFacts: ISet[RFAFact] = isetEmpty
    val argSlots = args.map(arg=>VarSlot(arg, isBase = false))
    val paramTyps = calleeMethod.getParamTypes
    for(i <- 0 to argSlots.size - 1){
      val argSlot = argSlots(i)
      paramTyps(i) match {
        case ot: ObjectType =>
          val argValues = s.pointsToSet(argSlot, currentContext)
          val influencedFields = 
            if(LibSideEffectProvider.isDefined)
              LibSideEffectProvider.getInfluencedFields(i, calleeMethod.getSignature)
            else Set(Constants.ALL_FIELD)
          argValues.foreach{
            ins => 
              for(f <- influencedFields) {
                val ins = UnknownInstance(ot, currentContext)
                val fs = FieldSlot(ins, f)
                genFacts += RFAFact(fs, ins)
              }
          }
        case pt: PrimitiveType =>
      }
    }
//    killFacts ++= ReachingFactsAnalysisHelper.getRelatedHeapFacts(argValues, s)
    val retTyp = calleeMethod.getReturnType
    retTyp match {
      case ot: ObjectType =>
        retVars.foreach{
          retVar =>
            val slot = VarSlot(retVar, isBase = false)
            val value = 
              if(retTyp.name == "java.lang.String") PTAPointStringInstance(currentContext)
              else UnknownInstance(ot, currentContext)
            genFacts += RFAFact(slot, value)
        }
      case _ =>
    }
    (genFacts, killFacts)
  }

  def getUnknownObjectForClinit(calleeMethod: JawaMethod, currentContext: Context): ISet[RFAFact] = {
    var result: ISet[RFAFact] = isetEmpty
    val record = calleeMethod.getDeclaringClass
    record.getDeclaredStaticObjectTypeFields.foreach{
      field =>
        result += RFAFact(StaticFieldSlot(field.FQN), UnknownInstance(field.getType.asInstanceOf[ObjectType], currentContext))
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
              case ne: NameExp => VarSlot(ne.name.name, isBase = true)
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
                VarSlot(ine.name.name, isBase = true)
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
  
  private def resolvePTAResultAccessExp(ae: AccessExp, typ: JawaType, currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult) = {
    val fqn = ASTUtil.getFieldFQN(ae, typ)
    val baseSlot = ae.exp match {
      case ne: NameExp => VarSlot(ne.name.name, isBase = true)
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
                 fact.s.asInstanceOf[FieldSlot].fieldName.equals("FIELD.ALL") &&
                 fqn.typ.isInstanceOf[ObjectType]) {
                val uIns = UnknownInstance(fqn.typ.asInstanceOf[ObjectType], fact.v.defSite)
                ptaresult.addInstance(fieldSlot, currentContext, uIns)
              }
          }
        }
    }
  }
  
  private def resolvePTAResultIndexingExp(ie: IndexingExp, currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult) = {
    val baseSlot = ie.exp match {
      case ine: NameExp =>
        VarSlot(ine.name.name, isBase = true)
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
        else if(ins.isInstanceOf[UnknownInstance]){
          val arraySlot = ArraySlot(ins)
          val temp = s.filter { fact => fact.s == arraySlot }.map{
            f => 
              ptaresult.addInstance(arraySlot, currentContext, f.v)
              f.v
          }
          if(temp.isEmpty){
            val uIns = UnknownInstance(ObjectType(ins.typ.typ, ins.typ.dimensions-1), currentContext)
            ptaresult.addInstance(arraySlot, currentContext, uIns)
          }
        }
        else{
          val arraySlot = ArraySlot(ins)
          s.filter { fact => fact.s == arraySlot }.map( f => ptaresult.addInstance(arraySlot, currentContext, f.v))
        }
    }
  }
  
  def updatePTAResultRHSs(rhss: List[Exp], typ: Option[JawaType], currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult) = {
    rhss.foreach{
      rhs=>
        updatePTAResultExp(rhs, typ, currentContext, s, ptaresult)
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
  
  def updatePTAResultExp(exp: Exp, typ: Option[JawaType], currentContext: Context, s: ISet[RFAFact], ptaresult: PTAResult): Unit = {
    exp match{
      case be: BinaryExp =>
        updatePTAResultExp(be.left, typ, currentContext, s, ptaresult)
        updatePTAResultExp(be.right, typ, currentContext, s, ptaresult)
      case ne: NameExp =>
        val slot = getNameSlotFromNameExp(ne, typ, false)
        slot match {
          case cs: ClassSlot => 
            val ci = ClassInstance(typ.get.asInstanceOf[ObjectType], currentContext)
            ptaresult.addInstance(cs, currentContext, ci)
          case ss: StaticFieldSlot =>
            s.filter { fact => fact.s == ss }.map( f => ptaresult.addInstance(ss, currentContext, f.v))
          case vs: VarSlot =>
            s.filter { fact => fact.s == vs }.map( f => ptaresult.addInstance(slot, currentContext, f.v))
        }
      case ae: AccessExp =>
        resolvePTAResultAccessExp(ae, typ.get, currentContext, s, ptaresult)
      case ie: IndexingExp =>
        resolvePTAResultIndexingExp(ie, currentContext, s, ptaresult)
      case ce: CastExp =>
        ce.exp match{
          case ice: NameExp =>
            val slot = VarSlot(ice.name.name, false)
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
        val slot = VarSlot(ne.name.name, false)
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
            VarSlot(ine.name.name, true)
          case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
        }
        val baseValue = ptaresult.pointsToSet(baseSlot, currentContext)
        baseValue.map{
          ins =>
            if(ins.isNull){}
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

  def processLHSs(lhss: List[Exp], typ: Option[JawaType], currentContext: Context, ptaresult: PTAResult): IMap[Int, IMap[PTASlot, Boolean]] = {
    val result = mmapEmpty[Int, MMap[PTASlot, Boolean]]
    var i = -1
    lhss.foreach{
      key=>
        i += 1
        key match{
          case ne: NameExp =>
            val slot = getNameSlotFromNameExp(ne, typ, false)
            result.getOrElseUpdate(i, mmapEmpty)(slot) = true
          case ae: AccessExp =>
            val fieldFQN = ASTUtil.getFieldFQN(ae, typ.get)
            val baseSlot = ae.exp match {
              case ne: NameExp => VarSlot(ne.name.name, true)
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
                VarSlot(ine.name.name, true)
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
  
  def processRHSs(rhss: List[Exp], typ: Option[JawaType], currentContext: Context, ptaresult: PTAResult): Map[Int, Set[Instance]] = {
    val result = mmapEmpty[Int, Set[Instance]]
    var i = -1
    rhss.foreach{
      rhs=>
        i += 1
        rhs match{
          case ne: NameExp =>
            val slot = getNameSlotFromNameExp(ne, typ, false)
            var value: ISet[Instance] = ptaresult.pointsToSet(slot, currentContext)   
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
                PTAInstance(ObjectType(name, dimensions), currentContext.copy, false)
              }
            val value: ISet[Instance] = Set(ins)
            result(i) = value
          case ae: AccessExp =>
            val fieldFQN = ASTUtil.getFieldFQN(ae, typ.get)
            val baseSlot = ae.exp match {
              case ne: NameExp => getNameSlotFromNameExp(ne, typ, true)
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
                  if(ins.isInstanceOf[UnknownInstance] && fieldFQN.typ.isInstanceOf[ObjectType]) {
                    fieldValue += UnknownInstance(fieldFQN.typ.asInstanceOf[ObjectType], currentContext)
                  }
                  result(i) = fieldValue
                }
            }
          case ie: IndexingExp =>
            val baseSlot = ie.exp match {
              case ine: NameExp =>
                getNameSlotFromNameExp(ine, typ, true)
              case _ => throw new RuntimeException("Wrong exp: " + ie.exp)
            }
            val baseValue: ISet[Instance] = ptaresult.pointsToSet(baseSlot, currentContext)
            baseValue.map{
              ins =>
                if(ins.isNull){}
                else if(ins.isInstanceOf[UnknownInstance]){
                  val arraySlot = ArraySlot(ins)
                  val arrayValue: MSet[Instance] = msetEmpty
                  arrayValue ++= ptaresult.pointsToSet(arraySlot, currentContext)
                  val originalType = ins.typ
                  if(originalType.dimensions == 0) throw new RuntimeException("Some problem must be happened for " + ins + " because indexing cannot happen on 0 dimension object. @" + currentContext)
                  val newType = ObjectType(originalType.typ, originalType.dimensions - 1)
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
            val casttyp: JawaType = ASTUtil.getTypeFromTypeSpec(ce.typeSpec)
            
            val insopt = 
              if(casttyp.typ == "java.lang.String" && casttyp.dimensions == 0){
                Some(PTAPointStringInstance(currentContext.copy))
              } else if (casttyp.isInstanceOf[ObjectType]) {
                Some(PTAInstance(casttyp.asInstanceOf[ObjectType], currentContext.copy, false))
              } else None
              
            insopt match {
              case Some(ins) =>
                ce.exp match{
                  case ne: NameExp =>
                    val slot = getNameSlotFromNameExp(ne, typ, false)
                    val value: ISet[Instance] = ptaresult.pointsToSet(slot, currentContext)
                    result(i) = value.map{
                      v =>
                        if(v.isInstanceOf[UnknownInstance]){
                          UnknownInstance(ins.typ, v.defSite.copy)
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
  
  def getNameSlotFromNameExp(ne: NameExp, typ: Option[JawaType], isBase: Boolean): NameSlot = {
    val name = ne.name.name
    if(name == Constants.CONST_CLASS) ClassSlot(typ.get.asInstanceOf[ObjectType])
    else if(name.startsWith("@@")) StaticFieldSlot(new FieldFQN(name.replace("@@", ""), typ.get))
    else VarSlot(name, isBase)
  }
}