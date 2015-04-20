/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.util

import org.sireum.jawa.JawaClass
import org.sireum.jawa.JawaMethod
import org.sireum.jawa.Center
import org.sireum.jawa.Type
import org.sireum.jawa.util.StringFormConverter
import org.sireum.util._
import org.sireum.jawa.MethodInvisibleException

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object CallHandler {
	/**
	 * get callee procedure from Center. Input: .equals:(Ljava/lang/Object;)Z
	 */
//	def getCalleeMethod(from : JawaClass, pSubSig : String) : JawaMethod = {
//	  Center.getClassHierarchy.resolveConcreteDispatch(from, pSubSig) match{
//  	  case Some(ap) => ap
//  	  case None => Center.getMethodWithoutFailing(Center.UNKNOWN_PROCEDURE_SIG)
//  	}
//	}
	
	/**
	 * check and get virtual callee procedure from Center. Input: .equals:(Ljava/lang/Object;)Z
	 */
	def getVirtualCalleeMethod(fromType : Type, pSubSig : String) : JawaMethod = {
	  val name =
	  	if(Center.isJavaPrimitiveType(fromType)) Center.DEFAULT_TOPLEVEL_OBJECT  // any array in java is an Object, so primitive type array is an object, object's method can be called
	  	else fromType.name
	  val from = Center.resolveClass(name, Center.ResolveLevel.HIERARCHY)
	  Center.getClassHierarchy.resolveConcreteDispatch(from, pSubSig)
	}
  
  /**
   * check and get virtual callee procedure from Center. Input: .equals:(Ljava/lang/Object;)Z
   */
  def getUnknownVirtualCalleeMethods(baseType : Type, pSubSig : String) : Set[JawaMethod] = {
    val baseName =
      if(Center.isJavaPrimitiveType(baseType)) Center.DEFAULT_TOPLEVEL_OBJECT  // any array in java is an Object, so primitive type array is an object, object's method can be called
      else baseType.name  
    val baseRec = Center.resolveClass(baseName, Center.ResolveLevel.HIERARCHY)
    Center.getClassHierarchy.resolveAbstractDispatch(baseRec, pSubSig)
  }
	
	/**
	 * check and get super callee procedure from Center. Input: Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z
	 */
	def getSuperCalleeMethod(pSig : String) : JawaMethod = {
	  val fromType = StringFormConverter.getClassTypeFromMethodSignature(pSig)
	  val pSubSig = StringFormConverter.getSubSigFromMethodSig(pSig)
	  val from = Center.resolveClass(fromType.name, Center.ResolveLevel.HIERARCHY)
	  Center.getClassHierarchy.resolveConcreteDispatch(from, pSubSig)
	}
	
	/**
	 * check and get static callee procedure from Center. Input: Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z
	 */
	def getStaticCalleeMethod(procSig : String) : JawaMethod = {
	  val recType = StringFormConverter.getClassTypeFromMethodSignature(procSig)
	  val pSubSig = Center.getSubSigFromMethodSig(procSig)
	  val from = Center.resolveClass(recType.name, Center.ResolveLevel.HIERARCHY)
	  Center.getClassHierarchy.resolveConcreteDispatch(from, pSubSig)
	}
	
	/**
	 * check and get direct callee procedure from Center. Input: Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z
	 */
	def getDirectCalleeMethod(procSig : String) : JawaMethod = {
	  val pSubSig = Center.getSubSigFromMethodSig(procSig)
	  val recType = StringFormConverter.getClassTypeFromMethodSignature(procSig)
	  val rec = Center.resolveClass(recType.name, Center.ResolveLevel.HIERARCHY)
	  if(rec.isUnknown){
	    this.synchronized{
		    Center.getMethod(procSig) match {
			    case Some(ap) => ap
			    case None => 
			      val ap = new JawaMethod
			      ap.init(procSig)
			      ap.setUnknown
			      rec.addMethod(ap)
			      ap
			  }
	    }
	  } else {
	    rec.getMethod(pSubSig)
	  }
	}
	
	def resolveSignatureBasedCall(callSig : String, typ : String) : ISet[JawaMethod] = {
	  val result : MSet[JawaMethod] = msetEmpty
	  val recName = Center.getClassNameFromMethodSignature(callSig)
    val subSig = Center.getSubSigFromMethodSig(callSig)
    val rec = Center.resolveClass(recName, Center.ResolveLevel.HIERARCHY)
    if(!rec.isUnknown){
		  typ match{
		    case "interface" =>
		      require(rec.isInterface)
	        Center.getClassHierarchy.getAllImplementersOf(rec).foreach{
	          record =>
	            if(record.isConcrete){
		            val fromType = StringFormConverter.getTypeFromName(record.getName)
		            var callee  : JawaMethod = null 
                try{
                  callee = getVirtualCalleeMethod(fromType, subSig)
                } catch {
                  case pe : MethodInvisibleException =>
                    println(pe.getMessage)
                  case a : Throwable =>
                    throw a
                }
                if(callee != null)
                  result += callee
	            }
	        }
	      case "virtual" =>
	        require(!rec.isInterface)
	        Center.getClassHierarchy.getAllSubClassesOfIncluding(rec).foreach{
	          record =>
	            if(record.isConcrete){
	            	val fromType = StringFormConverter.getTypeFromName(record.getName)
		            var callee  : JawaMethod = null 
                try{
                  callee = getVirtualCalleeMethod(fromType, subSig)
                } catch {
                  case pe : MethodInvisibleException =>
                    println(pe.getMessage)
                  case a : Throwable =>
                    throw a
                }
                if(callee != null)
                  result += callee
	            }
	        }
	      case "super" =>
	        result += getSuperCalleeMethod(callSig)
	      case "direct" =>
	        result += getDirectCalleeMethod(callSig)
	      case "static" =>
	      	result += getStaticCalleeMethod(callSig)
	    }
    }
	  result.toSet
	}
}