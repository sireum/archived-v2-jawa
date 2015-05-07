/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.interactive

import org.sireum.util._
import org.sireum.jawa.sjc.JavaKnowledge
import org.sireum.jawa.sjc.JawaType
import org.sireum.jawa.sjc.AccessFlag
import org.sireum.jawa.sjc.Signature
import org.sireum.jawa.sjc.ResolveLevel
import org.sireum.jawa.sjc.ObjectType

/**
 * This class is an jawa representation of a pilar method. It can belong to JawaClass.
 * You can also construct it manually. 
 * 
 * @constructor create a jawa method
 * @param declaringClass The declaring class of this method
 * @param name name of the method. e.g. stackState
 * @param thisOpt this param of the method, if the method is static or native it is None
 * @param params List of param name with its type of the method
 * @param returnTyp return type of the method
 * @param accessFlags access flags of this field
 * @param thrownExceptions exceptions throwing by this method
 * 
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
case class JawaMethod(declaringClass: JawaClass, 
                      name: String,
                      thisOpt: Option[String],
                      params: ISeq[(String, JawaType)], 
                      returnType: JawaType, 
                      accessFlags: Int) extends JavaKnowledge with ResolveLevel {
  
  import JawaMethod._
  
  final val TITLE = "JawaMethod"
	var DEBUG: Boolean = false
	
  require(this.declaringClass != null &&
          this.name != null &&
          this.thisOpt != null &&
          this.params != null &&
          this.returnType != null &&
          this.accessFlags >= 0) // NON-NIL
  
  require(!this.name.isEmpty())
  require(!(isStatic || isNative) && this.thisOpt.isDefined)
  
  def getDeclaringClass: JawaClass = this.declaringClass
  
  getDeclaringClass.addMethod(this)
  
	/**
	 * name of the method. e.g. equals
	 */
	def getName: String = name
	
  private val signature: Signature = generateSignature(this)
  
	/**
	 * signature of the method. e.g. Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z
	 */
	def getSignature: Signature = this.signature
	
	/**
	 * sub-signature of the method. e.g. equals:(Ljava/lang/Object;)Z
	 */
	def getSubSignature: String = {
    getSignature.getSubSignature
  }
	
  def getThisName: String = {
    require(!isStatic && !isNative)
    this.thisOpt.get
  }
  
  def getThisType: JawaType = {
    require(!isStatic && !isNative)
    getDeclaringClass.getType
  }
  
  /**
   * list of parameters. e.g. List((v1, java.lang.Object), (v2, java.lang.String))
   */
  def getParams: ISeq[(String, JawaType)] = this.params
  
	/**
	 * list of parameter types. e.g. List(java.lang.Object, java.lang.String)
	 */
	def getParamTypes: ISeq[JawaType] = getParams.map(_._2)
	
	/**
	 * list of parameter names
	 */
	def getParamNames: ISeq[String] = getParams.map(_._1)
	
  /**
   * get i'th parameter of this method
   */
  def getParam(i: Int): (String, JawaType) = getParams(i)
  
  /**
   * get i'th parameter's type of this method
   */
  def getParamType(i: Int): JawaType = getParamTypes(i)
  
  /**
   * get i'th parameter's name of this method
   */
  def getParamName(i: Int): String = getParamNames(i)
  
	/**
	 * return type. e.g. boolean
	 */
	
	def getReturnType: JawaType = this.returnType
	
	/**
   * the access flags integer represent for this method
   */
  def getAccessFlags: Int = 0
  
  /**
   * exceptions thrown by this method
   */
  protected val thrownExceptions: MSet[JawaClass] = msetEmpty
  
  /**
   * Data structure to store all information about a catch clause
   * LocUri should always looks like "L?[0-9a-f]+"
   */
  case class ExceptionHandler(exception: JawaClass, fromTarget: String, toTarget: String, jumpTo: String){
	  def handleException(exc: JawaClass, locUri: String): Boolean = {
	    (exception == exc || exception.isChildOf(exc)) && withInScope(locUri)
	  }
	  def withInScope(locUri: String): Boolean = {
	    getLocation(fromTarget) <= getLocation(locUri) && getLocation(locUri) <= getLocation(toTarget)
	  }
	  def getLocation(locUri: String): Int = {
	    val loc = locUri.substring(locUri.lastIndexOf("L") + 1)
    	Integer.getInteger(loc, 16)
	  }
	}
  
  /**
   * exception handlers
   */
  protected val exceptionHandlers: MList[ExceptionHandler] = mlistEmpty
  
  /**
   * represents if the method is unknown.
   */
  protected var unknown: Boolean = false
  
  /**
   * hold the body symbol table of this method
   */
  
//  protected var methodBody: MethodBody = null
  
	/**
	 * get access flag string
	 */
	def getAccessFlagString = AccessFlag.toString(getAccessFlags)
	
		
	/**
	 * set method body
	 */
	
//	def setMethodBody(pb: MethodBody) = this.methodBody = pb
	
	/**
	 * retrieve code belong to this method
	 */
	def retrieveCode = if(!isUnknown) JawaCodeSource.getMethodCode(getSignature) else throw new RuntimeException("Trying to retreive body code for a unknown method: " + this)
	
  /**
   * resolve current method to body level
   */
//  def resolveBody = {
//    if(getDeclaringClass.resolvingLevel >= ResolveLevel.BODY) throw new RuntimeException(getSignature +" is already resolved to " + this.resolvingLevel)
//    if(isUnknown) throw new RuntimeException(getSignature + " is a unknown method so cannot be resolved to body.")
//    val pb = JawaResolver.resolveMethodBody(getSignature)
//    setMethodBody(pb)
//    setResolvingLevel(Center.ResolveLevel.BODY)
//    getDeclaringClass.updateResolvingLevel
//  }
  
  /**
   * resolve current method to body level
   */
  
//  def tryResolveBody = {
//    if(isUnknown) throw new RuntimeException("Trying to get the body for a unknown method: " + this)
//    if(!checkLevel(Center.ResolveLevel.BODY)) resolveBody
//  }
  
	/**
	 * get method body
	 */
	
//	def getMethodBody = {
//	  tryResolveBody
//	  this.methodBody
//	}
  
  /**
   * check method body available or not
   */
  
//  def hasMethodBody = this.methodBody != null
  
  /**
   * clear method body
   */
  
//  def clearMethodBody = this.methodBody = null
  
  /**
   * Adds exception which can be thrown by this method
   */
  def addExceptionIfAbsent(exc: JawaClass) = {
    if(!throwException(exc)) addException(exc)
  }
  
  /**
   * Adds exception thrown by this method
   */
  def addException(exc: JawaClass) = {
    if(DEBUG) println("Adding Exception: " + exc)
    if(throwException(exc)) throw new RuntimeException("already throwing exception: " + exc)
    this.thrownExceptions += exc
  }
  
  /**
   * set exception with details
   */
  def addExceptionHandler(excName: String, fromTarget: String, toTarget: String, jumpTo: String) = {
    val recType: ObjectType = if(excName == "any") JAVA_TOPLEVEL_OBJECT_TYPE else getTypeFromName(excName).asInstanceOf[ObjectType]
    val exc = getDeclaringClass.global.resolveClass(recType, ResolveLevel.HIERARCHY)
    val handler = ExceptionHandler(exc, fromTarget, toTarget, jumpTo)
    if(DEBUG) println("Adding Exception Handler: " + handler)
    if(this.exceptionHandlers.contains(handler)) throw new RuntimeException("already have exception handler: " + handler)
    addExceptionIfAbsent(exc)
    this.exceptionHandlers += handler
  }
  
  /**
   * removes exception from this method
   */
  def removeException(exc: JawaClass) = {
    if(DEBUG) println("Removing Exception: " + exc)
    if(!throwException(exc)) throw new RuntimeException("does not throw exception: " + exc)
    this.thrownExceptions -= exc
    this.exceptionHandlers --= this.exceptionHandlers.filter(_.exception != exc)
  }
  
  /**
   * throws this exception or not?
   */
  def throwException(exc: JawaClass) = this.thrownExceptions.contains(exc)
  
  /**
   * get thrown exception target location
   */
  def getThrownExcetpionTarget(exc: JawaClass, locUri: String): Option[String] = {
    this.exceptionHandlers.find(_.handleException(exc, locUri)).map(_.jumpTo)
  }
  
  /**
   * set exceptions for this method
   */
  def setExceptions(excs: Set[JawaClass]) = {
    this.thrownExceptions.clear()
    this.thrownExceptions ++= excs
  }
  
  /**
   * get exceptions
   */
  def getExceptions: ISet[JawaClass] = this.thrownExceptions.toSet
  
  /**
   * return true if this method is concrete which means it is not abstract nor native nor unknown
   */
  def isConcrete: Boolean = !isAbstract && !isNative && !isUnknown
    
  /**
   * return true if this method is abstract
   */
  
  def isAbstract: Boolean = AccessFlag.isAbstract(this.accessFlags)
  
  /**
   * return true if this method is native
   */
  
  def isNative: Boolean = AccessFlag.isNative(this.accessFlags)
  
  /**
   * return true if this method is static
   */
  
  def isStatic: Boolean = AccessFlag.isStatic(this.accessFlags)
  
  /**
   * return true if this method is private
   */
  
  def isPrivate: Boolean = AccessFlag.isPrivate(this.accessFlags)
  
  /**
   * return true if this method is public
   */
  
  def isPublic: Boolean = AccessFlag.isPublic(this.accessFlags)
  
  /**
   * return true if this method is protected
   */
  
  def isProtected: Boolean = AccessFlag.isProtected(this.accessFlags)
  
  /**
   * return true if this method is final
   */
  
  def isFinal: Boolean = AccessFlag.isFinal(this.accessFlags)
  
  /**
   * return true if this method is synchronized
   */
  
  def isSynchronized: Boolean = AccessFlag.isSynchronized(this.accessFlags)
  
  /**
   * return true if this method is synthetic
   */
  
  def isSynthetic: Boolean = AccessFlag.isSynthetic(this.accessFlags)
  
  /**
   * return true if this method is constructor
   */
  
  def isConstructor: Boolean = AccessFlag.isConstructor(this.accessFlags)
  
  /**
   * return true if this method is declared_synchronized
   */
  
  def isDeclaredSynchronized: Boolean = AccessFlag.isDeclaredSynchronized(this.accessFlags)
  
  /**
   * return true if this method is main method
   */
  
  def isMain: Boolean = isPublic && isStatic && getSubSignature == "main:([Ljava/lang/string;)V"
    
  /**
   * return false if this method is a special one with empty body. e.g. A.class
   */
    
  def isUnknown: Boolean = this.unknown
  
  /**
   * set the reality of the method. e.g. for A.class we set the reality as false
   */
  def setUnknown = this.unknown = true
    
  /**
   * return true if this method is a class initializer or main function
   */
  def isEntryMethod = {
    if(isStatic && name == getDeclaringClass.staticInitializerName) true
    else isMain
  }
  
  def printDetail = {
    println("--------------AmandroidMethod--------------")
    println("name: " + getName)
    println("signature: " + getSignature)
    println("subSignature: " + getSubSignature)
    println("declaringClass: " + getDeclaringClass)
    println("paramTypes: " + getParamTypes)
    println("accessFlags: " + getAccessFlagString)
    println("----------------------------")
  }
  
  override def toString: String = getSignature.toString
  
}