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
import org.sireum.jawa.Center
import org.sireum.pilar.ast.LocationDecl
import org.sireum.pilar.ast.CallJump
import org.sireum.pilar.ast.NameExp

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object BackwardCallChain {
  
  def getReachableMethodsBySubSig(apiSubSig : String, par : Boolean) : Set[JawaMethod] = {
    var result : ISet[JawaMethod] = isetEmpty
	  val ps : Set[JawaMethod] = Center.getApplicationClasses.map(_.getMethods).reduce(iunion[JawaMethod])
	  val workList = mlistEmpty[String]
    val processed = msetEmpty[String]
	  result ++=
		  (if (par) ps.par else ps).filter{
		    proc =>
		      var flag : Boolean = false
		      if(proc.isConcrete){
		        if(!proc.hasMethodBody) proc.resolveBody
		        val body = proc.getMethodBody
		        flag = body.locations.map{
		          loc =>
		            visitLocUseSubSig(apiSubSig, loc)
		        }.exists(_ == true)
		      }
		      if(flag) workList += proc.getSignature
		      flag
		  }
  	processed ++= workList
    while(!workList.isEmpty){
      val sig = workList.remove(0)
      println(sig)
      processed += sig
      val tmp =
			  (if (par) ps.par else ps).filter{
			    proc =>
			      var flag : Boolean = false
			      if(proc.isConcrete){
			        if(!proc.hasMethodBody) proc.resolveBody
			        val body = proc.getMethodBody
			        flag = body.locations.map{
			          loc =>
			            visitLoc(sig, loc)
			        }.exists(_ == true)
			      }
			      flag
			  }
      workList ++= tmp.map(_.getSignature).filter(!processed.contains(_)).toList
      result ++= tmp
    }
	  result
  }
  
	def getReachableMethods(apiSigs : Set[String], par : Boolean) : Map[String, Set[JawaMethod]] = {
	  var result : Map[String, Set[JawaMethod]] = Map()
	  result ++= 
	    (if (par) apiSigs.par else apiSigs).map{
		    sig =>
		      (sig, getReachableMethods(sig, par))
		  }
	  result
  }
	
	def getReachableMethods(apiSig : String, par : Boolean) : Set[JawaMethod] = {
	  var result : ISet[JawaMethod] = isetEmpty
	  if(Center.getApplicationClasses.isEmpty) {
	    return Set()
	  }
	  val ps : Set[JawaMethod] = Center.getApplicationClasses.map(_.getMethods).reduce(iunion[JawaMethod])
	  
    val workList = mlistEmpty[String]
	  val processed = msetEmpty[String]
    workList += apiSig
    while(!workList.isEmpty){
      val sig = workList.remove(0)
      processed += sig
      val tmp =
			  (if (par) ps.par else ps).filter{
			    proc =>
			      var flag : Boolean = false
			      if(proc.isConcrete){
			        if(!proc.hasMethodBody) proc.resolveBody
			        val body = proc.getMethodBody
			        flag = body.locations.map{
			          loc =>
			            visitLoc(sig, loc)
			        }.exists(_ == true)
			      }
			      flag
			  }
      workList ++= tmp.map(_.getSignature).filter(!processed.contains(_)).toList
      result ++= tmp
    }
	  result
	}
	
	def visitLoc(apiSig : String, loc : LocationDecl) : Boolean = {
	  val apiRecName = Center.getClassNameFromMethodSignature(apiSig)
	  val apiSubSig = Center.getSubSigFromMethodSig(apiSig)
	  val apiRec = Center.resolveClass(apiRecName, Center.ResolveLevel.HIERARCHY)
	  var found = false
	  val visitor = Visitor.build({
      case t : CallJump if t.jump.isEmpty =>
        val sig = t.getValueAnnotation("signature") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => ""
        }
        val recName = Center.getClassNameFromMethodSignature(sig)
        val subSig = Center.getSubSigFromMethodSig(sig)
        val rec = Center.resolveClass(recName, Center.ResolveLevel.HIERARCHY)
        val typ = t.getValueAnnotation("type") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => ""
        }
        typ match{
          case "virtual" | "interface" | "super" =>
            if(apiSubSig == subSig){ 
              if(rec.isInterface){
                if(apiRec.isInterface) found = Center.getClassHierarchy.isClassRecursivelySubInterfaceOfIncluding(apiRec, rec)
                else found = Center.getClassHierarchy.getAllImplementersOf(rec).contains(apiRec)
              } else {
                if(!apiRec.isInterface) found = Center.getClassHierarchy.isClassRecursivelySubClassOfIncluding(apiRec, rec)
              }
            }
          case "direct" | "static" =>
            found = (sig == apiSig)
        }
        false
    })
    
    visitor(loc)
    found
	}
	
	def visitLocUseSubSig(apiSubSig : String, loc : LocationDecl) : Boolean = {
	  var found = false
	  val visitor = Visitor.build({
      case t : CallJump if t.jump.isEmpty =>
        val sig = t.getValueAnnotation("signature") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => ""
        }
        val subSig = Center.getSubSigFromMethodSig(sig)
        found = (subSig == apiSubSig)
        false
    })
    visitor(loc)
    found
	}
	
//	def getAllCallerSignatures()
	
}