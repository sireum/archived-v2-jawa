/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.pta.reachingFactsAnalysis.model

import org.sireum.jawa._
import org.sireum.util._
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.pta._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object MapModel {
	def isMap(r : JawaClass) : Boolean = {
    if(r.isApplicationClass) false
    else {
      val map = r.global.getClassOrResolve(new ObjectType("java.util.Map"))
      val res = r.global.getClassHierarchy.getAllImplementersOf(map).contains(r)
      res
    }
  }
	
	private def getPointStringToRet(retVar : String, currentContext : Context): RFAFact = {
    val newThisValue = PTAPointStringInstance(currentContext.copy)
    RFAFact(VarSlot(retVar, false, false), newThisValue)	 
	}
	  
	private def cloneMap(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    require(args.size >0)
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  thisValue.map{s => RFAFact(VarSlot(retVar, false, false), s.clone(currentContext))}
  }
	
	private def getMapEntrySetFactToRet(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    require(args.size >0)
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val strValue = thisValue.map{ins => s.pointsToSet(FieldSlot(ins, "entrys"), currentContext)}.reduce(iunion[Instance])
	  val rf = ReachingFactsAnalysisHelper.getReturnFact(ObjectType("java.util.HashSet", 0), retVar, currentContext).get
	  result += rf
	  result ++= strValue.map{s => RFAFact(FieldSlot(rf.v, "items"), s)}
	  result
  }
	
	private def getMapKeySetToRet(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    require(args.size >0)
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val strValue = thisValue.map{ins => s.pointsToSet(FieldSlot(ins, "entrys"), currentContext)}.reduce(iunion[Instance])
	  val rf = ReachingFactsAnalysisHelper.getReturnFact(ObjectType("java.util.HashSet", 0), retVar, currentContext).get
	  result += rf
	  strValue.foreach{
	    s =>
	      if(s.isInstanceOf[PTATupleInstance])
	      	  result += RFAFact(FieldSlot(rf.v, "items"), s.asInstanceOf[PTATupleInstance].left)
	  }
	  result
  }
	
	private def getMapValuesToRet(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    require(args.size >0)
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val strValue = thisValue.map{ins => s.pointsToSet(FieldSlot(ins, "entrys"), currentContext)}.reduce(iunion[Instance])
	  val rf = ReachingFactsAnalysisHelper.getReturnFact(ObjectType("java.util.HashSet", 0), retVar, currentContext).get
	  result += rf
	  result ++= strValue.map{
	    s => 
	      require(s.isInstanceOf[PTATupleInstance])
	      RFAFact(FieldSlot(rf.v, "items"), s.asInstanceOf[PTATupleInstance].right)
	  }
	  result
  }
	
	private def getMapValue(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  val result = msetEmpty[RFAFact]
    require(args.size >1)
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val keySlot = VarSlot(args(1), false, true)
	  val keyValue = s.pointsToSet(keySlot, currentContext)
    if(!thisValue.isEmpty){
  	  val entValue = thisValue.map{ins => s.pointsToSet(FieldSlot(ins, "entrys"), currentContext)}.reduce(iunion[Instance])
  	  entValue.foreach{
  	    v =>
  	      require(v.isInstanceOf[PTATupleInstance])
  	      if(keyValue.exists { kIns => kIns === v.asInstanceOf[PTATupleInstance].left }){
  	        result += (RFAFact(VarSlot(retVar, false, false), v.asInstanceOf[PTATupleInstance].right))
  	      }
  	  }
    }
	  result.toSet
  } 
	
	private def putMapValue(s : PTAResult, args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  val result = msetEmpty[RFAFact]
    require(args.size >2)
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val keySlot = VarSlot(args(1), false, true)
	  val keyValue = s.pointsToSet(keySlot, currentContext)
	  val valueSlot = VarSlot(args(2), false, true)
	  val valueValue = s.pointsToSet(valueSlot, currentContext)
	  val entrys = msetEmpty[Instance]
	  keyValue.foreach{
	    kv =>
	      valueValue.foreach{
	        vv =>
            thisValue.foreach{
              ins => entrys += PTATupleInstance(kv, vv, ins.defSite)
            }
	      }
	  }
	  thisValue.foreach{
	    ins =>
	      result ++= entrys.map(e => RFAFact(FieldSlot(ins, "entrys"), e))
	  }
	  result.toSet
  }
	
	private def putAllMapValues(s : PTAResult, args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    require(args.size >1)
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val slot2 = VarSlot(args(1), false, true)
	  val value2 = s.pointsToSet(slot2, currentContext)
	  thisValue.foreach{
	    ins =>
	      value2.foreach{
	        e => 
	          val ents = s.pointsToSet(FieldSlot(e, "entrys"), currentContext)
	          result ++= ents.map(RFAFact(FieldSlot(ins, "entrys"), _))
	      }
	  }
	  result
  }
	
	def doMapCall(s : PTAResult, p : JawaMethod, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  p.getSignature.getSubSignature match{
		  case "clear:()V" =>
		  case "clone:()Ljava/lang/Object;" =>
		    require(retVars.size == 1)
		    newFacts ++= cloneMap(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "entrySet:()Ljava/util/Set;" =>
		    require(retVars.size == 1)
		    newFacts ++= getMapEntrySetFactToRet(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "get:(Ljava/lang/Object;)Ljava/lang/Object;" =>
		    require(retVars.size == 1)
		    newFacts ++= getMapValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "keySet:()Ljava/util/Set;" =>
		    require(retVars.size == 1)
		    newFacts ++= getMapKeySetToRet(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "put:(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;" =>
		    newFacts ++= putMapValue(s, args, currentContext)
		    byPassFlag = false
		  case "putAll:(Ljava/util/Map;)V" =>
		    newFacts ++= putAllMapValues(s, args, currentContext)
		    byPassFlag = false
		  case "remove:(Ljava/lang/Object;)Ljava/lang/Object;" =>
		    require(retVars.size == 1)
		    newFacts ++= getMapValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "values:()Ljava/util/Collection;" =>
		    require(retVars.size == 1)
		    newFacts ++= getMapValuesToRet(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case _ =>
	  }
	  (newFacts, delFacts, byPassFlag)
	}
}