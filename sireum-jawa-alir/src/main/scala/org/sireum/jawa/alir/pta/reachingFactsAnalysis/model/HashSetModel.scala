/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.pta.reachingFactsAnalysis.model

import org.sireum.jawa.JawaClass
import org.sireum.util._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.jawa.alir.pta._
import org.sireum.jawa.JawaMethod
import org.sireum.jawa.alir.Context
import org.sireum.jawa.ObjectType

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object HashSetModel {
	def isHashSet(r : JawaClass) : Boolean = r.getName == "java.util.HashSet"
	  
  private def addItemToHashSetField(s : PTAResult, args : List[String], currentContext : Context) : ISet[RFAFact] ={
	  require(args.size > 1)
	  var newfacts = isetEmpty[RFAFact]
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValues = s.pointsToSet(thisSlot, currentContext)
	  val paramSlot = VarSlot(args(1), false, true)
	  val paramValues = s.pointsToSet(paramSlot, currentContext)
	  thisValues.foreach{
      ins =>
        newfacts ++= paramValues.map{p=>RFAFact(FieldSlot(ins, "java.util.HashSet.items"), p)}
    }
	  newfacts 
	}
  
  private def cloneHashSetToRet(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    require(args.size >0)
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  thisValue.map{s => RFAFact(VarSlot(retVar, false, false), s.clone(currentContext))}
  }
  
  def doHashSetCall(s : PTAResult, p : JawaMethod, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  p.getSignature.signature match{
      case "Ljava/util/HashSet;.<init>:()V" =>
//        newFacts ++= initializeHashSetField(s, args, currentContext)
		  case "Ljava/util/HashSet;.<init>:(I)V" =>
//		    newFacts ++= initializeHashSetField(s, args, currentContext)
		  case "Ljava/util/HashSet;.<init>:(IF)V" =>
//		    newFacts ++= initializeHashSetField(s, args, currentContext)
		  case "Ljava/util/HashSet;.<init>:(Ljava/util/Collection;)V" =>
//		    newFacts ++= initializeHashSetField(s, args, currentContext)
		  case "Ljava/util/HashSet;.<init>:(Ljava/util/HashMap;)V" =>
//		    newFacts ++= initializeHashSetField(s, args, currentContext)
		  case "Ljava/util/HashSet;.add:(Ljava/lang/Object;)Z" =>
		    newFacts ++= addItemToHashSetField(s, args, currentContext)
		    byPassFlag = false
		  case "Ljava/util/HashSet;.clear:()V" =>
		  case "Ljava/util/HashSet;.clone:()Ljava/lang/Object;" =>
		    require(retVars.size == 1)
		    newFacts ++= cloneHashSetToRet(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "Ljava/util/HashSet;.contains:(Ljava/lang/Object;)Z" =>
		  case "Ljava/util/HashSet;.createBackingMap:(IF)Ljava/util/HashMap;" =>
		    require(retVars.size == 1)
		    ReachingFactsAnalysisHelper.getReturnFact(ObjectType("java.util.HashMap", 0), retVars(0), currentContext) match{
		      case Some(fact) => newFacts += fact
		      case None =>
		    }
		    byPassFlag = false
		  case "Ljava/util/HashSet;.isEmpty:()Z" =>
		  case "Ljava/util/HashSet;.iterator:()Ljava/util/Iterator;" =>
		  case "Ljava/util/HashSet;.readObject:(Ljava/io/ObjectInputStream;)V" =>
		  case "Ljava/util/HashSet;.remove:(Ljava/lang/Object;)Z" =>
		  case "Ljava/util/HashSet;.size:()I" =>
		  case "Ljava/util/HashSet;.writeObject:(Ljava/io/ObjectOutputStream;)V" =>
		  case _ =>
    }
    (newFacts, delFacts, byPassFlag)
  }
}