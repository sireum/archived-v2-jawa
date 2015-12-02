/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa

import org.sireum.util._

object JawaType {
  def generateType(typ: String, dimensions: Int): JawaType = {
    if(dimensions == 0 && JavaKnowledge.isJavaPrimitive(typ)) PrimitiveType(typ)
    else ObjectType(typ, dimensions)
  }
  
  def addDimensions(typ: JawaType, dimensions: Int): JawaType = {
    if(dimensions > 0) {
      ObjectType(typ.typ, typ.dimensions + dimensions)
    } else typ
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait JawaType extends JavaKnowledge {
  def typ: String
  def name: String
  def jawaName: String
  def simpleName: String
  def canonicalName: String
  def dimensions: Int
  def isArray: Boolean
}

final case class PrimitiveType(typ: String) extends JawaType {
  require(isJavaPrimitive(this))
  def name: String = typ
  def simpleName: String = name
  def jawaName: String = name
  def canonicalName: String = name
  def dimensions: Int = 0
  def isArray: Boolean = false
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class ObjectType(pkg: JawaPackage, typ: String, dimensions: Int) extends JawaType {
  def this(pkg: JawaPackage, typ: String) = this(pkg, typ, 0)
  def this(pkgAndTyp: String, dimensions: Int) = (
      )
  
  require(!isJavaPrimitive(typ) || dimensions != 0)
  def isArray = dimensions > 0
  def name: String = formatObjectTypeToObjectName(this)
  def simpleName: String = {
    var res = canonicalName.substring(canonicalName.lastIndexOf(".") + 1)
    res
  }
  def jawaName: String = {
    val base = typ
    assign(base, dimensions, "[]", false)
  }
  def canonicalName: String = {
    val base = typ.replaceAll("\\$", ".")
    assign(base, dimensions, "[]", false)
  }
  def pkg: String = {
    if(isJavaPrimitive(typ) || isArray) null
    else typ.substring(0, typ.lastIndexOf(".")).intern()
  }
  def toUnknown: ObjectType = ObjectType(typ + "*", dimensions)
  /**
   * The result looks like:
   * input: java.lang.wfg.W$F$G$H
   * output: List(java.lang.wfg.W$F$G, java.lang.wfg.W$F, java.lang.wfg.W)
   */
  def getEnclosingTypes: List[ObjectType] = {
    val result: MList[ObjectType] = mlistEmpty
    if(isArray) return result.toList
    else if(typ.contains("$")){
      var outer = typ.substring(0, typ.lastIndexOf("$"))
      while(outer.contains("$")) {
        result += ObjectType(outer, 0)
        outer = outer.substring(0, outer.lastIndexOf("$"))
      } 
      result += ObjectType(outer, 0)
    }
    result.toList
  }
  
  override def toString: String = {
    name
  }
}

case class InvalidTypeException(msg: String) extends RuntimeException(msg)