/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa


/**
 * This class is an jawa representation of a pilar field. It should belong to a JawaClass. 
 * 
 * @constructor create a jawa field
 * @param declaringClass The declaring class of this field
 * @param name name of the field. e.g. stackState
 * @param typ JawaType of the field
 * @param accessFlags access flags of this field
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
case class JawaField(declaringClass: JawaClass, name: String, typ: JawaType, accessFlags: Int) extends JawaElement with JavaKnowledge {
  import JawaField._

    /**
     * construct a jawa field instance
     */
  def this(declaringClass: JawaClass, name: String, typ: JawaType, accessString: String) = {
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
  def FQN: FieldFQN = generateFieldFQN(declaringClass.getType, name, typ)

  /**
   * field type
   */
  def getType: JawaType = typ

  /**
   * return true if the field is object type
   */
  def isObject: Boolean = !isJavaPrimitive(typ)
  
  def isConcrete: Boolean = true

  override def toString(): String = FQN.toString()

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
