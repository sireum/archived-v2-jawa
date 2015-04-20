/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa

import org.sireum.jawa.util._
import org.sireum.jawa.MessageCenter._
import org.sireum.util._

/**
 * This class is an jawa representation of a pilar method. It can belong to JawaClass.
 * You can also construct it manually. 
 * 
 * 
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
class JawaMethod extends ResolveLevel with PropertyProvider {

  final val TITLE = "JawaMethod"
	var DEBUG : Boolean = false
	
	/**
	 * short name of the method. e.g. equals
	 */
	
	protected var shortName : String = null
	
	/**
	 * full name of the method. e.g. java.lang.Object.equals
	 */
	
	protected var name : String = null
	
	/**
	 * signature of the method. e.g. Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z
	 */
	
	protected var signature : String = null
	
	/**
	 * sub-signature of the method. e.g. equals:(Ljava/lang/Object;)Z
	 */
	
	protected var subSignature : String = null
	
	/**
	 * list of parameter types. e.g. List(java.lang.Object, java.lang.String)
	 */
	
	protected var paramTyps : List[Type] = List()
	
	/**
	 * list of parameter names
	 */
	
	protected var paramNames : List[String] = List()
	
	/**
	 * return type. e.g. boolean
	 */
	
	protected var returnTyp : Type = null
	
	/**
	 * declaring class of this method
	 */
	
	protected var declaringClass : JawaClass = null
	
	/**
   * the access flags integer represent for this method
   */
  
  protected var accessFlags : Int = 0
  
  /**
   * exceptions thrown by this method
   */
  
  protected var exceptions : Set[JawaClass] = Set()
  
  /**
   * Data structure to store all information about a catch clause
   * LocUri should always looks like "L?[0-9a-f]+"
   */
  
  case class ExceptionHandler(exception : JawaClass, fromTarget : String, toTarget : String, jumpTo : String){
	  def handleException(exc : JawaClass, locUri : String) : Boolean = {
	    (exception == exc || exception.isChildOf(exc)) && withInScope(locUri)
	  }
	  def withInScope(locUri : String) : Boolean = {
	    getLocation(fromTarget) <= getLocation(locUri) && getLocation(locUri) <= getLocation(toTarget)
	  }
	  def getLocation(locUri : String) : Int = {
	    val loc = locUri.substring(locUri.lastIndexOf("L") + 1)
    	Integer.getInteger(loc, 16)
	  }
	}
  
  /**
   * exception handlers
   */
  
  protected var exceptionHandlers : IList[ExceptionHandler] = ilistEmpty
  
  /**
   * represents if the method is unknown.
   */
  
  protected var unknown : Boolean = false
  
  /**
   * hold the body symbol table of this method
   */
  
  protected var procBody : MethodBody = null
  
  /**
   * supply property
   */
  val propertyMap = mlinkedMapEmpty[Property.Key, Any]
  
  /**
	 * is it declared in some JawaClass?
	 */
	
	def isDeclared : Boolean = declaringClass != null
  
  /**
   * return hash code to provide structural equality
   */
  
  def strEquHashCode : Int = this.returnTyp.hashCode() * 101 + this.accessFlags * 17 + this.name.hashCode()
  
  /**
   * when you construct an amandroid method instance, call this init function first
   */
  
  def init(name : String, sig : String, paramTyps : List[Type], returnTyp : Type, accessFlags : Int, thrownExceptions : List[JawaClass]): JawaMethod = {
	  setName(name)
	  setSignature(sig)
	  this.paramTyps ++= paramTyps
	  this.returnTyp = returnTyp
	  this.accessFlags = accessFlags
	  
	  if(exceptions.isEmpty || !thrownExceptions.isEmpty){
	    exceptions ++= thrownExceptions
	  }
	  this
	}
  
  /**
   * when you construct a amandroid method instance, call this init function first
   */
  
  def init(name : String, sig : String, paramTyps : List[Type], returnTyp : Type, accessFlags : Int): JawaMethod = {
	  init(name, sig, paramTyps, returnTyp, accessFlags, List())
	}
  
  /**
   * when you construct a amandroid method instance, call this init function first
   */
  
  def init(name : String, sig : String, paramTyps : List[Type], returnTyp : Type): JawaMethod = {
	  init(name, sig, paramTyps, returnTyp, 0, List())
	}
  
  /**
   * when you construct a amandroid method instance, call this init function first
   */
  
  def init(sig : String): JawaMethod = {
    val name = StringFormConverter.getMethodNameFromMethodSignature(sig)
    val recName = StringFormConverter.getClassNameFromMethodSignature(sig)
    val sigP = new SignatureParser(sig).getParamSig
    val paramTyps = sigP.getParameterTypes
    val returnTyp = sigP.getReturnType
	  init(name, sig, paramTyps, returnTyp, 0, List())
	}
  
  /**
   * when you construct a amandroid method instance, call this init function first
   */
  
  def init(name : String, sig : String): JawaMethod = {
    val sigP = new SignatureParser(sig).getParamSig
    val recName = StringFormConverter.getClassNameFromMethodSignature(sig)
    val paramTyps = sigP.getParameterTypes
    val returnTyp = sigP.getReturnType
	  init(name, sig, paramTyps, returnTyp, 0, List())
	}
  
  /**
   * get short name of this method
   */
  
  def getShortName : String = this.shortName
  
  def getShortName(name : String) : String = {
    if(isFullName(name)) name.substring(name.lastIndexOf('.') + 1)
    else throw new RuntimeException("method name not correct: " + name)
  }

  def isFullName(name : String) : Boolean = name.lastIndexOf('.') > 0
  
  /**
   * get name of this method
   */
  
  def getName : String = this.name
  
  /**
	 * set declaring class of this field
	 */
  
  def setDeclaringClass(declClass : JawaClass) = if(declClass != null) this.declaringClass = declClass
	
  /**
	 * clear declaring class of this field
	 */
  
  def clearDeclaringClass = this.declaringClass = null
  
  /**
   * return the class which declares the current method
   */
  
  def getDeclaringClass = {
    if(!isDeclared) throw new RuntimeException("no declaring class: " + getName)
    declaringClass
  }
  
  /**
   * set the name of this method
   */
  
  def setName(name : String) = {
    val wasDeclared = isDeclared
    val oldDeclaringClass = declaringClass
    if(wasDeclared) oldDeclaringClass.removeMethod(this)
    this.name = name
    this.shortName = getShortName(name)
    if(wasDeclared) oldDeclaringClass.addMethod(this)
  }
  
  /**
   * set signature of this method
   */
  
  def setSignature(sig : String) = {
    if(checkSignature(sig)){
	    this.signature = sig
	    this.subSignature = getSubSignature
    } else throw new RuntimeException("not a full-qualified signature: " + sig)
  }
  
  protected def checkSignature(sig : String) = sig.lastIndexOf('.') > 0
  
  /**
   * get signature of this method
   */
  
  def getSignature : String = this.signature
  
  /**
   * get sub-signature of this method
   */
  
  def getSubSignature : String = {
    if(this.signature != null) {
      this.signature.substring(this.signature.lastIndexOf('.') + 1, this.signature.length())
    } else {
      generateSubSignature
    }
  }
  
  /**
   * generate signature of this method
   */
  
  def generateSignature : String = {
    val sb : StringBuffer = new StringBuffer
    if(this.declaringClass != null){
	    val dc = this.declaringClass
	    sb.append(StringFormConverter.formatTypeToSigForm(dc.getName))
	    sb.append("." + generateSubSignature)
	    sb.toString().intern()
    } else throw new RuntimeException("not declared: " + this.name)
  }
  
  /**
   * generate sub-signature of this method
   */
  
  def generateSubSignature : String = {
    val sb : StringBuffer = new StringBuffer
    val rt = getReturnType
    val pts = getParamTypes
    sb.append(this.shortName + ":(")
    for(i <- 0 to pts.size - 1){
      if(i != 0){
	      val pt = pts(i) 
	      sb.append(StringFormConverter.formatTypeToSigForm(pt.typ))
      }
    }
    sb.append(StringFormConverter.formatTypeToSigForm(rt.typ))
    sb.toString().intern()
  }
  
  /**
   * set return type of this method
   */
  
  def setReturnType(typ : Type) = {
    val wasDeclared = isDeclared
    val oldDeclaringClass = declaringClass
    if(wasDeclared) oldDeclaringClass.removeMethod(this)
    this.returnTyp = typ
    this.signature = generateSignature
    this.subSignature = generateSubSignature
    if(wasDeclared) oldDeclaringClass.addMethod(this)
  }
  
  /**
   * set parameter types of this method
   */
  
  def setParameterTypes(typs : List[Type]) = {
    val wasDeclared = isDeclared
    val oldDeclaringClass = declaringClass
    if(wasDeclared) oldDeclaringClass.removeMethod(this)
    this.paramTyps = typs
    this.signature = generateSignature
    this.subSignature = generateSubSignature
    if(wasDeclared) oldDeclaringClass.addMethod(this)
  }
  
  /**
   * get return type of this method
   */
  
  def getReturnType = this.returnTyp
  
  /**
   * get paramTypes of this method
   */
  
  def getParamTypes = {
    if(!isStatic){
      StringFormConverter.getTypeFromName(getDeclaringClass.getName) :: this.paramTyps
    } else this.paramTyps
  }
  
  /**
   * get i'th parameter's type of this method
   */
  
  def getParamType(i : Int) = getParamTypes(i)
  
  /**
   * set parameter names of this method
   */
  
  def setParameterNames(names : List[String]) = {
    this.paramNames = names
  }
  
  /**
   * get paramTypes of this method
   */
  
  def getParamNames = this.paramNames
  
  /**
   * get i'th parameter's type of this method
   */
  
  def getParamName(i : Int) = this.paramNames(i)
  
  /**
	 * return the access flags for this method
	 */
	
	def getAccessFlags = accessFlags
	
	/**
	 * get access flag string
	 */
	
	def getAccessFlagString = AccessFlag.toString(getAccessFlags)
	
	/**
	 * sets the access flags for this method
	 */
	
	def setAccessFlags(af : Int) = this.accessFlags = af
	
	/**
	 * sets the access flags for this method
	 */
	
	def setAccessFlags(str : String) = this.accessFlags = AccessFlag.getAccessFlags(str)
	
	/**
	 * set method body
	 */
	
	def setMethodBody(pb : MethodBody) = this.procBody = pb
	
	/**
	 * retrieve code belong to this method
	 */
	
	def retrieveCode = if(!isUnknown) JawaCodeSource.getMethodCode(getSignature) else throw new RuntimeException("Trying to retreive body code for a unknown method: " + this)
	
  /**
   * resolve current method to body level
   */
  
  def resolveBody = {
    if(this.resolvingLevel >= Center.ResolveLevel.BODY) throw new RuntimeException(getSignature +" is already resolved to " + this.resolvingLevel)
    if(isUnknown) throw new RuntimeException(getSignature + " is a unknown method so cannot be resolved to body.")
    val pb = JawaResolver.resolveMethodBody(getSignature)
    setMethodBody(pb)
    setResolvingLevel(Center.ResolveLevel.BODY)
    getDeclaringClass.updateResolvingLevel
  }
  
  /**
   * resolve current method to body level
   */
  
  def tryResolveBody = {
    if(isUnknown) throw new RuntimeException("Trying to get the body for a unknown method: " + this)
    if(!checkLevel(Center.ResolveLevel.BODY)) resolveBody
  }
  
	/**
	 * get method body
	 */
	
	def getMethodBody = {
	  tryResolveBody
	  this.procBody
	}
  
  /**
   * check method body available or not
   */
  
  def hasMethodBody = this.procBody != null
  
  /**
   * clear method body
   */
  
  def clearMethodBody = this.procBody = null
  
  /**
   * Adds exception which can be thrown by this method
   */
  
  def addExceptionIfAbsent(exc : JawaClass) = {
    if(!throwsException(exc)) addException(exc)
  }
  
  /**
   * Adds exception thrown by this method
   */
  
  def addException(exc : JawaClass) = {
    if(DEBUG) println("Adding Exception: " + exc)
    if(throwsException(exc)) throw new RuntimeException("already throwing exception: " + exc)
    this.exceptions += exc
  }
  
  /**
   * set exception with details
   */
  
  def addExceptionHandler(excName : String, fromTarget : String, toTarget : String, jumpTo : String) = {
    val recName = if(excName == "any") Center.DEFAULT_TOPLEVEL_OBJECT else excName
    val exc = Center.resolveClass(recName, Center.ResolveLevel.HIERARCHY)
    val handler = ExceptionHandler(exc, fromTarget, toTarget, jumpTo)
    if(DEBUG) println("Adding Exception Handler: " + handler)
    if(this.exceptionHandlers.contains(handler)) throw new RuntimeException("already have exception handler: " + handler)
    addExceptionIfAbsent(exc)
    this.exceptionHandlers ::= handler
  }
  
  /**
   * removes exception from this method
   */
  
  def removeException(exc : JawaClass) = {
    if(DEBUG) println("Removing Exception: " + exc)
    if(!throwsException(exc)) throw new RuntimeException("does not throw exception: " + exc)
    this.exceptions -= exc
    this.exceptionHandlers = this.exceptionHandlers.filter(_.exception != exc)
  }
  
  /**
   * throws this exception or not?
   */
  
  def throwsException(exc : JawaClass) = this.exceptions.contains(exc)
  
  /**
   * get thrown exception target location
   */
  
  def getThrownExcetpionTarget(exc : JawaClass, locUri : String) : Option[String] = {
    this.exceptionHandlers.foreach{
      han =>
        if(han.handleException(exc, locUri)) return Some(han.jumpTo)
    }
    err_msg_detail(TITLE, "Given exception " + exc + " throw from " + locUri + ", cannot find a handler to handle it.")
    None
  }
  
  /**
   * set exceptions for this method
   */
  
  def setExceptions(excs : Set[JawaClass]) = this.exceptions = excs
  
  /**
   * get exceptions
   */
  
  def getExceptions = this.exceptions
  
  /**
   * return true if this method is concrete which means it is not abstract nor native nor unknown
   */
  
  def isConcrete = !isAbstract && !isNative && !isUnknown
    
  /**
   * return true if this method is abstract
   */
  
  def isAbstract : Boolean = AccessFlag.isAbstract(this.accessFlags)
  
  /**
   * return true if this method is native
   */
  
  def isNative : Boolean = AccessFlag.isNative(this.accessFlags)
  
  /**
   * return true if this method is static
   */
  
  def isStatic : Boolean = AccessFlag.isStatic(this.accessFlags)
  
  /**
   * return true if this method is private
   */
  
  def isPrivate : Boolean = AccessFlag.isPrivate(this.accessFlags)
  
  /**
   * return true if this method is public
   */
  
  def isPublic : Boolean = AccessFlag.isPublic(this.accessFlags)
  
  /**
   * return true if this method is protected
   */
  
  def isProtected : Boolean = AccessFlag.isProtected(this.accessFlags)
  
  /**
   * return true if this method is final
   */
  
  def isFinal : Boolean = AccessFlag.isFinal(this.accessFlags)
  
  /**
   * return true if this method is synchronized
   */
  
  def isSynchronized : Boolean = AccessFlag.isSynchronized(this.accessFlags)
  
  /**
   * return true if this method is synthetic
   */
  
  def isSynthetic : Boolean = AccessFlag.isSynthetic(this.accessFlags)
  
  /**
   * return true if this method is constructor
   */
  
  def isConstructor : Boolean = AccessFlag.isConstructor(this.accessFlags)
  
  /**
   * return true if this method is declared_synchronized
   */
  
  def isDeclaredSynchronized : Boolean = AccessFlag.isDeclaredSynchronized(this.accessFlags)
  
  /**
   * return true if this method is main method
   */
  
  def isMain : Boolean = isPublic && isStatic && this.subSignature == "main:([Ljava/lang/string;)V"
    
  /**
   * return false if this method is a special one with empty body. e.g. A.class
   */
    
  def isUnknown : Boolean = this.unknown
  
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
    println("procName: " + getName)
    println("shortName: " + getShortName)
    println("signature: " + getSignature)
    println("subSignature: " + getSubSignature)
    println("declaringClass: " + getDeclaringClass)
    println("paramTypes: " + getParamTypes)
    println("accessFlags: " + getAccessFlagString)
    println("----------------------------")
  }
  
  override def toString : String = getSignature
  
}