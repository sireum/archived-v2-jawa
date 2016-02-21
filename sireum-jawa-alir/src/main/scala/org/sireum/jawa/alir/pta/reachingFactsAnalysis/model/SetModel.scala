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
package org.sireum.jawa.alir.pta.reachingFactsAnalysis.model

import org.sireum.jawa.JawaClass
import org.sireum.util._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.jawa.alir.pta._
import org.sireum.jawa.JawaMethod
import org.sireum.jawa.alir.Context
import org.sireum.jawa.JawaType

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object SetModel {
	def isSet(r : JawaClass) : Boolean = {
    if(r.isApplicationClass) false
    else {
      val set = r.global.getClassOrResolve(new JawaType("java.util.Set"))
      r.global.getClassHierarchy.getAllImplementersOf(set).contains(r)  
    }
  }
	  
  private def addItemToSetField(s : PTAResult, args : List[String], currentContext : Context) : ISet[RFAFact] = {
	  require(args.size > 1)
	  var newfacts = isetEmpty[RFAFact]
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValues = s.pointsToSet(thisSlot, currentContext)
	  val paramSlot = VarSlot(args(1), false, true)
	  val paramValues = s.pointsToSet(paramSlot, currentContext)
	  thisValues.foreach{
      ins =>
        newfacts ++= paramValues.map{p=>RFAFact(FieldSlot(ins, "items"), p)}
    }
	  newfacts 
	}
  
  private def cloneSetToRet(s : PTAResult, args : List[String], retVar : String, currentContext : Context) : ISet[RFAFact] ={
    require(args.size >0)
    val thisSlot = VarSlot(args(0), false, true)
	  val thisValue = s.pointsToSet(thisSlot, currentContext)
	  thisValue.map{s => RFAFact(VarSlot(retVar, false, false), s.clone(currentContext))}
  }
  
  def doSetCall(s : PTAResult, p : JawaMethod, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  p.getSignature.getSubSignature match{
		  case "add:(Ljava/lang/Object;)Z" =>
		    newFacts ++= addItemToSetField(s, args, currentContext)
		    byPassFlag = false
		  case "clear:()V" =>
		  case "clone:()Ljava/lang/Object;" =>
		    require(retVars.size == 1)
		    newFacts ++= cloneSetToRet(s, args, retVars(0), currentContext)
		    byPassFlag = false
		  case "contains:(Ljava/lang/Object;)Z" =>
//		  case "Ljava/util/HashSet;.createBackingMap:(IF)Ljava/util/HashMap;" =>
//		    require(retVars.size == 1)
//		    ReachingFactsAnalysisHelper.getReturnFact(ObjectType("java.util.HashMap", 0), retVars(0), currentContext) match{
//		      case Some(fact) => newFacts += fact
//		      case None =>
//		    }
//		    byPassFlag = false
		  case "isEmpty:()Z" =>
		  case "iterator:()Ljava/util/Iterator;" =>
		  case "readObject:(Ljava/io/ObjectInputStream;)V" =>
		  case "remove:(Ljava/lang/Object;)Z" =>
		  case "size:()I" =>
		  case "writeObject:(Ljava/io/ObjectOutputStream;)V" =>
		  case _ =>
    }
    (newFacts, delFacts, byPassFlag)
  }
}
