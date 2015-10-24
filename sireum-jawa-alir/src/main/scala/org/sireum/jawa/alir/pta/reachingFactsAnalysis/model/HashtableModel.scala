/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.pta.reachingFactsAnalysis.model

import org.sireum.jawa.JawaClass
import org.sireum.jawa.JawaMethod
import org.sireum.util._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.jawa._
import org.sireum.jawa.alir.pta._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object HashtableModel {
	def isHashtable(r : JawaClass) : Boolean = r.getName == "java.util.Hashtable"
	
	private def getPointStringToRet(retVar : String, currentContext : Context): RFAFact = {
    val newThisValue = PTAPointStringInstance(currentContext.copy)
    RFAFact(VarSlot(retVar, false, false), newThisValue)	 
	}
	  
	private def cloneHashTable(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    require(args.size >0)
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  thisValue.map{s => RFAFact(VarSlot(retVar, false, false), s.clone(currentContext))}
  }
	
	private def getHashTableEntrySetFactToRet(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    require(args.size >0)
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val strValue = thisValue.map{ins => s.pointsToSet(FieldSlot(ins, "entrys"), currentContext)}.reduce(iunion[Instance])
	  val rf = ReachingFactsAnalysisHelper.getReturnFact(ObjectType("java.util.HashSet", 0), retVar, currentContext).get
	  result += rf
	  result ++= strValue.map{s => RFAFact(FieldSlot(rf.v, "java.util.HashSet.items"), s)}
	  result
  }
	
	private def getHashTableKeySetToRet(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
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
	      RFAFact(FieldSlot(rf.v, "java.util.HashSet.items"), s.asInstanceOf[PTATupleInstance].left)
	  }
	  result
  }
	
	private def getHashTableValuesToRet(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
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
	      RFAFact(FieldSlot(rf.v, "java.util.HashSet.items"), s.asInstanceOf[PTATupleInstance].right)
	  }
	  result
  }
	
	private def getHashTableValue(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    require(args.size >1)
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val keySlot = VarSlot(args(1), false, true)
	  val keyValue = s.pointsToSet(keySlot, currentContext)
	  val entValue = thisValue.map{ins => s.pointsToSet(FieldSlot(ins, "entrys"), currentContext)}.reduce(iunion[Instance])
	  entValue.foreach{
	    v =>
	      require(v.isInstanceOf[PTATupleInstance])
	      if(keyValue.exists { kIns => kIns === v.asInstanceOf[PTATupleInstance].left }){
	        result += (RFAFact(VarSlot(retVar, false, false), v.asInstanceOf[PTATupleInstance].right))
	      }
	  }
	  result
  }
	
	private def putHashTableValue(s : PTAResult, args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    require(args.size >2)
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val keySlot = VarSlot(args(1), false, true)
	  val keyValue = s.pointsToSet(keySlot, currentContext)
	  val valueSlot = VarSlot(args(2), false, true)
	  val valueValue = s.pointsToSet(valueSlot, currentContext)
	  var entrys = isetEmpty[Instance]
	  keyValue.foreach{
	    kv =>
	      valueValue.foreach{
	        vv =>
            thisValue.foreach{
              ins =>
                entrys += PTATupleInstance(kv, vv, ins.defSite)
            }
	          
	      }
	  }
	  thisValue.foreach{
	    ins =>
	      result ++= entrys.map(e => RFAFact(FieldSlot(ins, "entrys"), e))
	  }
	  result
  }
	
	private def putAllHashTableValues(s : PTAResult, args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  var result = isetEmpty[RFAFact]
    require(args.size >1)
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  val slot2 = VarSlot(args(1), false, true)
	  val value2 = s.pointsToSet(slot2, currentContext)
	  thisValue.foreach{
	    ins =>
	      result ++= value2.map(e => RFAFact(FieldSlot(ins, "entrys"), e))
	  }
	  result
  }
	  
	def doHashtableCall(s : PTAResult, p : JawaMethod, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  p.getSignature.signature match{
	    case "Ljava/util/Hashtable;.<clinit>:()V" =>
		  case "Ljava/util/Hashtable;.<init>:()V" =>
		  case "Ljava/util/Hashtable;.<init>:(I)V" =>
		  case "Ljava/util/Hashtable;.<init>:(IF)V" =>
		  case "Ljava/util/Hashtable;.<init>:(Ljava/util/Map;)V" =>
		  case "Ljava/util/Hashtable;.access$1100:(Ljava/util/Hashtable;Ljava/lang/Object;Ljava/lang/Object;)Z" =>
		  case "Ljava/util/Hashtable;.access$1200:(Ljava/util/Hashtable;Ljava/lang/Object;Ljava/lang/Object;)Z" =>
		  case "Ljava/util/Hashtable;.access$500:(Ljava/util/Hashtable;)I" =>
		  case "Ljava/util/Hashtable;.access$600:(Ljava/util/Hashtable;)[Ljava/util/Hashtable$HashtableEntry;" =>
		  case "Ljava/util/Hashtable;.access$800:(Ljava/util/Hashtable;)I" =>
		  case "Ljava/util/Hashtable;.capacityForInitSize:(I)I" =>
		  case "Ljava/util/Hashtable;.clear:()V" =>
		  case "Ljava/util/Hashtable;.clone:()Ljava/lang/Object;" =>
		    require(retVars.size == 1)
		    newFacts ++= cloneHashTable(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Ljava/util/Hashtable;.constructorPut:(Ljava/lang/Object;Ljava/lang/Object;)V" =>
		  case "Ljava/util/Hashtable;.constructorPutAll:(Ljava/util/Map;)V" =>
		  case "Ljava/util/Hashtable;.contains:(Ljava/lang/Object;)Z" =>
		  case "Ljava/util/Hashtable;.containsKey:(Ljava/lang/Object;)Z" =>
		  case "Ljava/util/Hashtable;.containsMapping:(Ljava/lang/Object;Ljava/lang/Object;)Z" =>
		  case "Ljava/util/Hashtable;.containsValue:(Ljava/lang/Object;)Z" =>
		  case "Ljava/util/Hashtable;.doubleCapacity:()[Ljava/util/Hashtable$HashtableEntry;" =>
		  case "Ljava/util/Hashtable;.elements:()Ljava/util/Enumeration;" =>
		  case "Ljava/util/Hashtable;.ensureCapacity:(I)V" =>
		  case "Ljava/util/Hashtable;.entrySet:()Ljava/util/Set;" =>
		    require(retVars.size == 1)
		    newFacts ++= getHashTableEntrySetFactToRet(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Ljava/util/Hashtable;.equals:(Ljava/lang/Object;)Z" =>
		  case "Ljava/util/Hashtable;.get:(Ljava/lang/Object;)Ljava/lang/Object;" =>
		    require(retVars.size == 1)
		    newFacts ++= getHashTableValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Ljava/util/Hashtable;.hashCode:()I" =>
		  case "Ljava/util/Hashtable;.isEmpty:()Z" =>
		  case "Ljava/util/Hashtable;.keySet:()Ljava/util/Set;" =>
		    require(retVars.size == 1)
		    newFacts ++= getHashTableKeySetToRet(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Ljava/util/Hashtable;.keys:()Ljava/util/Enumeration;" =>
		  case "Ljava/util/Hashtable;.makeTable:(I)[Ljava/util/Hashtable$HashtableEntry;" =>
		  case "Ljava/util/Hashtable;.put:(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;" =>
		    newFacts ++= putHashTableValue(s, args, currentContext)
		    byPassFlag = false
		  case "Ljava/util/Hashtable;.putAll:(Ljava/util/Map;)V" =>
		    newFacts ++= putAllHashTableValues(s, args, currentContext)
		    byPassFlag = false
		  case "Ljava/util/Hashtable;.readObject:(Ljava/io/ObjectInputStream;)V" =>
		  case "Ljava/util/Hashtable;.rehash:()V" =>
		  case "Ljava/util/Hashtable;.remove:(Ljava/lang/Object;)Ljava/lang/Object;" =>
		    require(retVars.size == 1)
		    newFacts ++= getHashTableValue(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Ljava/util/Hashtable;.removeMapping:(Ljava/lang/Object;Ljava/lang/Object;)Z" =>
		  case "Ljava/util/Hashtable;.size:()I" =>
		  case "Ljava/util/Hashtable;.toString:()Ljava/lang/String;" =>
		    require(retVars.size == 1)
		    newFacts += getPointStringToRet(retVars(0), currentContext)
		    byPassFlag = false
		  case "Ljava/util/Hashtable;.values:()Ljava/util/Collection;" =>
		    require(retVars.size == 1)
		    newFacts ++= getHashTableValuesToRet(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Ljava/util/Hashtable;.writeObject:(Ljava/io/ObjectOutputStream;)V" =>
	  }
	  (newFacts, delFacts, byPassFlag)
	}
}