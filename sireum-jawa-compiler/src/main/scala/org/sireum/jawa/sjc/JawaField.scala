/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc

/**
 * This class is an jawa representation of a pilar field. It should belong to a JawaClass. 
 * 
 * @constructor create a jawa field
 * @param declaringClass The declaring class of this field
 * @param name name of the field. e.g. stackState
 * @param typ type of the field
 * @param accessFlags access flags of this field
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class JawaField(declaringClass: JawaClass, name: String, typ: Type, accessFlags: Int) extends JavaKnowledge {
  import JawaField._
	
  /**
   * construct a jawa field instance
   */
	def this(declaringClass: JawaClass, name: String, typ: Type, accessString: String) = {
    this(declaringClass, name, typ, AccessFlag.getAccessFlags(accessString))
	}
  
  declaringClass.addField(this)
  
  def getDeclaringClass: JawaClass = this.declaringClass
  
  /**
   * Field name like: f
   */
  def getName: String = name
  
  /**
   * full qualified name of the field. e.g. java.lang.Throwable.stackState
   */
  def FQN: String = generateFQN(declaringClass, name)
	
  /**
   * field type
   */
  def getType: Type = typ
	
  def getAccessFlags: Int = this.accessFlags
  
	/**
	 * get field access flags in text form
	 */
	def getAccessFlagsStr: String = AccessFlag.toString(this.accessFlags)
	
	/**
	 * return true if the field is public
	 */
	def isPublic: Boolean = AccessFlag.isPublic(this.accessFlags)
	
	/**
	 * return true if the field is protected
	 */
	def isProtected: Boolean = AccessFlag.isProtected(this.accessFlags)
	
	/**
	 * return true if the field is private
	 */
	def isPrivate: Boolean = AccessFlag.isPrivate(this.accessFlags)
	
	/**
	 * return true if the field is static
	 */
	def isStatic: Boolean = AccessFlag.isStatic(this.accessFlags)
	
	/**
	 * return true if the field is final
	 */
	def isFinal: Boolean = AccessFlag.isFinal(this.accessFlags)
	
	/**
	 * return true if the field is object type
	 */
	def isObject: Boolean = !isJavaPrimitive(typ)
	
	override def toString(): String = FQN
	
	def printDetail = {
	  println("~~~~~~~~~~~~~JawaField~~~~~~~~~~~~~")
	  println("name: " + name)
	  println("FQN: " + FQN)
	  println("type: " + typ)
	  println("accessFlags: " + AccessFlag.toString(accessFlags))
	  println("declaringClass: " + declaringClass)
	  println("~~~~~~~~~~~~~~~~~~~~~~~~~~")
	}
	
}

object JawaField {
  /**
   * check if given string is field signature or not
   */
  def isFQN(str: String) = isValidFieldFQN(str)
  
  /**
   * generate signature of this field. input: ("java.lang.Throwable", "stackState") output: "java.lang.Throwable.stackState"
   */
  private def generateFieldFQN(className: String, name: String): String = {
    val sb = new StringBuffer
    sb.append(className + "." + name)
    sb.toString().intern()
  }
  
  /**
   * generate signature of this field
   */
  def generateFQN(ar: JawaClass, name: String): String = generateFieldFQN(ar.getName, name)
  
  /**
   * FQN of the field. e.g. java.lang.Throwable.stackState or @@java:lang:Enum.sharedConstantsCache
   */
  def isValidFieldFQN(fqn: String): Boolean = fqn.lastIndexOf('.') > 0
  
  /**
   * FQN of the field. e.g. java.lang.Throwable.stackState or @@java:lang:Enum.sharedConstantsCache
   */
  def isValidFieldName(name: String): Boolean = !name.contains('.')
  
  /**
   * get field name from field FQN. e.g. java.lang.Throwable.stackState -> stackState
   */
  def getFieldNameFromFieldFQN(fqn: String): String = {
    if(!isValidFieldFQN(fqn)) throw new RuntimeException("given field signature is not a valid form: " + fqn)
    else fqn.substring(fqn.lastIndexOf('.') + 1)
  }
  
  /**
   * get class name from field signature. e.g. java.lang.Throwable.stackState -> java.lang.Throwable
   * [Ljava.lang.String;.length -> [Ljava.lang.String;
   */
  def getClassNameFromFieldFQN(fqn: String): String = {
    if(!isValidFieldFQN(fqn)) throw new RuntimeException("given field signature is not a valid form: " + fqn)
    fqn.substring(0, fqn.lastIndexOf('.'))
  }
}