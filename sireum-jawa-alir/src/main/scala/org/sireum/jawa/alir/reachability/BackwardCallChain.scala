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
package org.sireum.jawa.alir.reachability

import org.sireum.util._
import org.sireum.jawa._
import org.sireum.jawa.alir.util.CallHandler

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object BackwardCallChain {
  class CallChain(val sig: Signature) {
    private val callerMap: MMap[Signature, MSet[Signature]] = mmapEmpty
    def addCallers(callee: Signature, callers: ISet[Signature]) = {
      callerMap.getOrElseUpdate(callee, msetEmpty) ++= callers
    }
    def getCallerMap: IMap[Signature, ISet[Signature]] = this.callerMap.map{case (k, v) => k -> v.toSet}.toMap
  }
  
  def getBackwardCallChain(global: Global, sig: Signature): CallChain = {
    global.resolveAllApplicationClasses
    val ps: ISet[JawaMethod] = global.getApplicationClasses.map(_.getDeclaredMethods).fold(isetEmpty)(iunion[JawaMethod]).filter(_.isConcrete)
    val calleeSigMethodMap: MMap[Signature, MSet[Signature]] = mmapEmpty
    ps.map {
      m =>
        val callees: MSet[JawaMethod] = msetEmpty
        val points = new PointsCollector().points(m.getSignature, m.getBody)
        points foreach {
          p =>
            p match {
              case pi: Point with Right with Invoke =>
                val typ = pi.invokeTyp
                val sig = pi.sig
                typ match {
                  case "super" =>
                    callees ++= CallHandler.getSuperCalleeMethod(global, sig)
                  case "direct" =>
                    callees ++= CallHandler.getDirectCalleeMethod(global, sig)
                  case "static" =>
                    callees ++= CallHandler.getStaticCalleeMethod(global, sig)
                  case "virtual" | "interface" | _ =>
                    callees ++= CallHandler.getUnknownVirtualCalleeMethods(global, sig.getClassType, sig.getSubSignature)
                }
            }
        }
        callees.foreach{
          callee =>
            calleeSigMethodMap.getOrElseUpdate(callee.getSignature, msetEmpty) += m.getSignature
        }
    }
    val result: CallChain = new CallChain(sig)
    val worklist: MList[Signature] = mlistEmpty
    val processed: MSet[Signature] = msetEmpty
    worklist += sig
    while(!worklist.isEmpty) {
      val worksig = worklist.remove(0)
      processed += worksig
      val callerSigs = calleeSigMethodMap.getOrElse(worksig, isetEmpty)
      result.addCallers(worksig, callerSigs.toSet)
      worklist ++= callerSigs.filterNot { x => processed.contains(x) }
    }
    result
  }
  
}
