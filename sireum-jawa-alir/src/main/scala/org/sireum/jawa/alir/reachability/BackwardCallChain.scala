/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.reachability

import org.sireum.jawa.JawaMethod
import scala.collection.GenSet
import org.sireum.util._
import org.sireum.pilar.ast.LocationDecl
import org.sireum.pilar.ast.CallJump
import org.sireum.pilar.ast.NameExp
import org.sireum.jawa.Signature
import org.sireum.jawa.Global

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object BackwardCallChain {
  
//	def getReachableMethods(apiSigs: Set[String], par: Boolean): Map[String, Set[JawaMethod]] = {
//	  var result: Map[String, Set[JawaMethod]] = Map()
//	  result ++= 
//	    (if (par) apiSigs.par else apiSigs).map{
//		    sig =>
//		      (sig, getReachableMethods(sig, par))
//		  }
//	  result
//  }
//	
//  /**
//   * Before calling this method please load all the classes you want to process into HIERACHY level.
//   */
//	def getReachableMethods(global: Global, apiSig: Signature, par: Boolean): Set[JawaMethod] = {
//	  var result: ISet[JawaMethod] = isetEmpty
//	  val ps: Set[JawaMethod] = global.getApplicationClasses.map(_.getMethods).reduce(iunion[JawaMethod])
//	  
//    val workList = mlistEmpty[String]
//	  val processed = msetEmpty[String]
//    workList += apiSig
//    while(!workList.isEmpty){
//      val sig = workList.remove(0)
//      processed += sig
//      val tmp =
//			  (if (par) ps.par else ps).filter{
//			    proc =>
//			      var flag: Boolean = false
//			      if(proc.isConcrete){
//			        if(!proc.hasMethodBody) proc.resolveBody
//			        val body = proc.getMethodBody
//			        flag = body.locations.map{
//			          loc =>
//			            visitLoc(sig, loc)
//			        }.exists(_ == true)
//			      }
//			      flag
//			  }
//      workList ++= tmp.map(_.getSignature).filter(!processed.contains(_)).toList
//      result ++= tmp
//    }
//	  result
//	}
	
}