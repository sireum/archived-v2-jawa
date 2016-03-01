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
import org.sireum.jawa.alir.callGraph.CallGraph
import org.sireum.jawa._
import org.sireum.jawa.alir.util.CallHandler
import org.sireum.jawa.alir.pta.PTAScopeManager

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object SignatureBasedCallGraph {
  
  def apply(
      global: Global, 
      entryPoints: ISet[Signature]): CallGraph = build(global, entryPoints)
      
  def build(
      global: Global, 
      entryPoints: ISet[Signature]): CallGraph = {
    global.resolveAllApplicationClasses
    val cg = new CallGraph
    entryPoints.foreach{
      ep =>
        val epmopt = global.getMethod(ep)
        epmopt match {
          case Some(epm) => 
            if(epm.isConcrete) {
              sbcg(global, epm, cg)
            }
          case None =>
        }
    }
    cg
  }
  
  private def sbcg(global: Global, ep: JawaMethod, cg: CallGraph) = {
    val worklist: MList[JawaMethod] = mlistEmpty // Make sure that all the method in the worklist are concrete.
    val processed: MSet[String] = msetEmpty
    worklist += ep
    while(!worklist.isEmpty) {
      val m = worklist.remove(0)
      processed += m.getSignature.signature
      val points = new PointsCollector().points(m.getSignature, m.getBody)
      points foreach {
        p =>
          p match {
            case pi: Point with Right with Invoke =>
              val typ = pi.invokeTyp
              val sig = pi.sig
              val callees: MSet[JawaMethod] = msetEmpty
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
              callees foreach {
                callee =>
                  cg.addCall(m.getSignature, callee.getSignature)
                  if(!processed.contains(callee.getSignature.signature) && !PTAScopeManager.shouldBypass(callee.getDeclaringClass) && callee.isConcrete) {
                    worklist += callee
                  }
              }
            case _ =>
          }
      }
    }
  }
}