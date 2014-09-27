/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.reachability

import org.sireum.jawa.JawaProcedure
import org.sireum.util._
import org.sireum.jawa.Center
import org.sireum.pilar.ast.LocationDecl
import org.sireum.pilar.ast.CallJump
import org.sireum.pilar.ast.NameExp
import org.sireum.jawa.JawaRecord
import org.sireum.jawa.alir.util.CallHandler

object SignatureBasedCallGraph {
  def getReachableProcedures(record : JawaRecord, wholeProcs : Set[JawaProcedure], par : Boolean) : Set[JawaProcedure] = {
    require(!record.isInterface && !record.isPhantom)
    getReachableProcedures(record.getProcedures, wholeProcs, par)
  }
  
	def getReachableProcedures(procedures : Set[JawaProcedure], appProcs : Set[JawaProcedure], par : Boolean) : Set[JawaProcedure] = {
	  var result : Set[JawaProcedure] = Set()
	  val processed = mlistEmpty[JawaProcedure]
	  var workList = isetEmpty[JawaProcedure]
    workList ++= procedures
    while(!workList.isEmpty){
      val tmp =
			  (if (par) workList else workList).map{
			    proc =>
			      if(proc.isConcrete){
			        if(!proc.hasProcedureBody) proc.resolveBody
			        val body = proc.getProcedureBody
			        body.locations.map{
			          loc =>
			            visitLoc(loc, appProcs)
			        }.reduce(iunion[JawaProcedure])
			      } else Set[JawaProcedure]()
			  }.reduce(iunion[JawaProcedure])
			processed ++= workList
			workList = tmp.filter(!processed.contains(_))
    }
	  result
  }
	
	
	def visitLoc(loc : LocationDecl, appProcs : Set[JawaProcedure]) : Set[JawaProcedure] = {
	  var result = Set[JawaProcedure]()
	  val visitor = Visitor.build({
      case t : CallJump if t.jump.isEmpty =>
        val sig = t.getValueAnnotation("signature") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => ""
        }
        val typ = t.getValueAnnotation("type") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => ""
        }
        result ++= CallHandler.resolveSignatureBasedCall(sig, typ)
        false
    })
    
    visitor(loc)
    result
	}
}