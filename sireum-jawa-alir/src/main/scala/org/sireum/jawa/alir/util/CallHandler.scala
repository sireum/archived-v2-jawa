/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.util

import org.sireum.jawa.JawaRecord
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.Center
import org.sireum.jawa.Type
import org.sireum.jawa.util.StringFormConverter

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object CallHandler {
	/**
	 * get callee procedure from Center. Input: .equals:(Ljava/lang/Object;)Z
	 */
	def getCalleeProcedure(from : JawaRecord, pSubSig : String) : JawaProcedure = {
	  Center.getRecordHierarchy.resolveConcreteDispatch(from, pSubSig) match{
  	  case Some(ap) => ap
  	  case None => Center.getProcedureWithoutFailing(Center.UNKNOWN_PROCEDURE_SIG)
  	}
	}
	
	/**
	 * check and get virtual callee procedure from Center. Input: .equals:(Ljava/lang/Object;)Z
	 */
	def getVirtualCalleeProcedure(fromType : Type, pSubSig : String) : JawaProcedure = {
	  val name =
	  	if(Center.isJavaPrimitiveType(fromType)) Center.DEFAULT_TOPLEVEL_OBJECT  // any array in java is an Object, so primitive type array is an object, object's method can be called
	  	else fromType.name	
	  val from = Center.resolveRecord(name, Center.ResolveLevel.HIERARCHY)
	  Center.getRecordHierarchy.resolveConcreteDispatch(from, pSubSig) match{
  	  case Some(ap) => ap
  	  case None => Center.getProcedureWithoutFailing(Center.UNKNOWN_PROCEDURE_SIG)
  	}
	}
	
	/**
	 * check and get super callee procedure from Center. Input: Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z
	 */
	def getSuperCalleeProcedure(pSig : String) : JawaProcedure = {
	  val fromType = StringFormConverter.getRecordTypeFromProcedureSignature(pSig)
	  val pSubSig = StringFormConverter.getSubSigFromProcSig(pSig)
	  val from = Center.resolveRecord(fromType.name, Center.ResolveLevel.HIERARCHY)
	  Center.getRecordHierarchy.resolveConcreteDispatch(from, pSubSig) match{
  	  case Some(ap) => ap
  	  case None => Center.getProcedureWithoutFailing(Center.UNKNOWN_PROCEDURE_SIG)
  	}
	}
	
	/**
	 * check and get static callee procedure from Center. Input: Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z
	 */
	def getStaticCalleeProcedure(procSig : String) : JawaProcedure = {
	  val recType = StringFormConverter.getRecordTypeFromProcedureSignature(procSig)
	  val pSubSig = Center.getSubSigFromProcSig(procSig)
	  val from = Center.resolveRecord(recType.name, Center.ResolveLevel.HIERARCHY)
	  Center.getRecordHierarchy.resolveConcreteDispatch(from, pSubSig) match{
  	  case Some(ap) => ap
  	  case None => Center.getProcedureWithoutFailing(Center.UNKNOWN_PROCEDURE_SIG)
  	}
	}
	
	/**
	 * check and get direct callee procedure from Center. Input: Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z
	 */
	def getDirectCalleeProcedure(procSig : String) : JawaProcedure = {
	  val pSubSig = Center.getSubSigFromProcSig(procSig)
	  val recType = StringFormConverter.getRecordTypeFromProcedureSignature(procSig)
	  val rec = Center.resolveRecord(recType.name, Center.ResolveLevel.HIERARCHY)
	  if(rec.isPhantom){
	    this.synchronized{
		    Center.getProcedure(procSig) match {
			    case Some(ap) => ap
			    case None => 
			      val ap = new JawaProcedure
			      ap.init(procSig)
			      ap.setPhantom
			      rec.addProcedure(ap)
			      ap
			  }
	    }
	  } else {
	    rec.getProcedure(pSubSig)
	  }
	}
	
	def resolveSignatureBasedCall(callSig : String, typ : String) : Set[JawaProcedure] = {
	  var result : Set[JawaProcedure] = Set()
	  val recName = Center.getRecordNameFromProcedureSignature(callSig)
    val subSig = Center.getSubSigFromProcSig(callSig)
    val rec = Center.resolveRecord(recName, Center.ResolveLevel.HIERARCHY)
    if(!rec.isPhantom){
		  typ match{
		    case "interface" =>
		      require(rec.isInterface)
	        Center.getRecordHierarchy.getAllImplementersOf(rec).foreach{
	          record =>
	            if(record.isConcrete){
		            val fromType = StringFormConverter.getTypeFromName(record.getName)
		            result += getVirtualCalleeProcedure(fromType, subSig)
	            }
	        }
	      case "virtual" =>
	        require(!rec.isInterface)
	        Center.getRecordHierarchy.getAllSubClassesOfIncluding(rec).foreach{
	          record =>
	            if(record.isConcrete){
	            	val fromType = StringFormConverter.getTypeFromName(record.getName)
		            result += getVirtualCalleeProcedure(fromType, subSig)
	            }
	        }
	      case "super" =>
	        result += getSuperCalleeProcedure(callSig)
	      case "direct" =>
	        result += getDirectCalleeProcedure(callSig)
	      case "static" =>
	      	result += getStaticCalleeProcedure(callSig)
	    }
    }
	  result
	}
}