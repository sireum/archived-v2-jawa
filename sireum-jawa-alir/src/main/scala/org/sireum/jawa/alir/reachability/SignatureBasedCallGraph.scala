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

import org.sireum.jawa.JawaMethod
import org.sireum.util._
import org.sireum.pilar.ast.LocationDecl
import org.sireum.pilar.ast.CallJump
import org.sireum.pilar.ast.NameExp
import org.sireum.jawa.JawaClass
import org.sireum.jawa.alir.util.CallHandler

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object SignatureBasedCallGraph {
//  def getReachableMethods(clazz : JawaClass, wholeProcs : Set[JawaMethod], par : Boolean) : Set[JawaMethod] = {
//    require(!clazz.isInterface && !clazz.isUnknown)
//    getReachableMethods(clazz.getMethods, wholeProcs, par)
//  }
//  
//	def getReachableMethods(procedures : Set[JawaMethod], appProcs : Set[JawaMethod], par : Boolean) : Set[JawaMethod] = {
//	  var result : Set[JawaMethod] = Set()
//	  val processed = mlistEmpty[JawaMethod]
//	  var workList = isetEmpty[JawaMethod]
//    workList ++= procedures
//    while(!workList.isEmpty){
//      val tmp =
//			  (if (par) workList else workList).map{
//			    proc =>
//			      if(proc.isConcrete){
//			        if(!proc.hasMethodBody) proc.resolveBody
//			        val body = proc.getMethodBody
//			        body.locations.map{
//			          loc =>
//			            visitLoc(loc, appProcs)
//			        }.reduce(iunion[JawaMethod])
//			      } else Set[JawaMethod]()
//			  }.reduce(iunion[JawaMethod])
//			processed ++= workList
//			workList = tmp.filter(!processed.contains(_))
//    }
//	  result
//  }
//	
//	
//	def visitLoc(loc : LocationDecl, appProcs : Set[JawaMethod]) : Set[JawaMethod] = {
//	  var result = Set[JawaMethod]()
//	  val visitor = Visitor.build({
//      case t : CallJump if t.jump.isEmpty =>
//        val sig = t.getValueAnnotation("signature") match {
//          case Some(s) => s match {
//            case ne : NameExp => ne.name.name
//            case _ => ""
//          }
//          case None => ""
//        }
//        val typ = t.getValueAnnotation("type") match {
//          case Some(s) => s match {
//            case ne : NameExp => ne.name.name
//            case _ => ""
//          }
//          case None => ""
//        }
//        result ++= CallHandler.resolveSignatureBasedCall(sig, typ)
//        false
//    })
//    
//    visitor(loc)
//    result
//	}
}
