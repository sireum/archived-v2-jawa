/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa.alir.pta.reachingFactsAnalysis.model

import org.sireum.jawa.JawaMethod
import org.sireum.util._
import org.sireum.jawa.JawaClass
import org.sireum.jawa.alir.Context
import org.sireum.alir.Slot
import org.sireum.jawa.alir.pta.Instance
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.jawa.alir.pta.PTAResult

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
trait ModelCallHandler {
  
  /**
   * return true if the given callee procedure needs to be modeled
   */
  def isModelCall(calleeProc: JawaMethod): Boolean = {
    val r = calleeProc.getDeclaringClass
    StringBuilderModel.isStringBuilder(r) ||
    StringModel.isString(r) ||
    ListModel.isList(r) ||
    SetModel.isSet(r) || 
    MapModel.isMap(r) ||
    ClassModel.isClass(r) ||
//    ConstructorModel.isConstructor(calleeProc) ||
    ObjectModel.isObject(r) ||
    NativeCallModel.isNativeCall(calleeProc) ||
    UnknownCallModel.isUnknownCall(calleeProc)
  }
      
  /**
   * instead of doing operation inside callee procedure's real code, we do it manually and return the result. 
   */
  def doModelCall[T](
      s: PTAResult,
      calleeProc: JawaMethod, 
      args: List[String], 
      retVars: Seq[String], 
      currentContext: Context,
      addition: Option[T])(implicit factory: RFAFactFactory): (ISet[RFAFact], ISet[RFAFact]) = {
    val hackVars = if(retVars.size != 1) retVars :+ "hack" else retVars
    
    var (newFacts, delFacts, byPassFlag) = caculateResult(s, calleeProc, args, hackVars, currentContext, addition)
    if(byPassFlag){
      val (newF, delF) = ReachingFactsAnalysisHelper.getUnknownObject(calleeProc, s, args, hackVars, currentContext)
      newFacts ++= newF
      delFacts ++= delF
    }
    (newFacts, delFacts)
  }

  def caculateResult[T](
      s: PTAResult, 
      calleeProc: JawaMethod, 
      args: List[String], 
      retVars: Seq[String], 
      currentContext: Context,
      addition: Option[T])(implicit factory: RFAFactFactory): (ISet[RFAFact], ISet[RFAFact], Boolean) = {
    val r = calleeProc.getDeclaringClass
    if(StringModel.isString(r)) StringModel.doStringCall(s, calleeProc, args, retVars, currentContext)
    else if(StringBuilderModel.isStringBuilder(r)) StringBuilderModel.doStringBuilderCall(s, calleeProc, args, retVars, currentContext)
    else if(ListModel.isList(r)) ListModel.doListCall(s, calleeProc, args, retVars, currentContext)
    else if(SetModel.isSet(r)) SetModel.doSetCall(s, calleeProc, args, retVars, currentContext)
    else if(MapModel.isMap(r)) MapModel.doMapCall(s, calleeProc, args, retVars, currentContext)
    else if(ClassModel.isClass(r)) ClassModel.doClassCall(s, calleeProc, args, retVars, currentContext)
//    else if(ConstructorModel.isConstructor(calleeProc)) ConstructorModel.doConstructorCall(s, calleeProc, args, retVars, currentContext)
    else if(ObjectModel.isObject(r)) ObjectModel.doObjectCall(s, calleeProc, args, retVars, currentContext)
    else if(NativeCallModel.isNativeCall(calleeProc)) NativeCallModel.doNativeCall(s, calleeProc, args, retVars, currentContext)
    else if(UnknownCallModel.isUnknownCall(calleeProc)) UnknownCallModel.doUnknownCall(s, calleeProc, args, retVars, currentContext)
    else throw new RuntimeException("given callee is not a model call: " + calleeProc)
  }
}

object NormalModelCallHandler extends ModelCallHandler
