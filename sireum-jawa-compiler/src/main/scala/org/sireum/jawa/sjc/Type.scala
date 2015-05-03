/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait Type extends JavaKnowledge {
	def typ: String
  def isArray: Boolean
}

final case class PrimitiveType(val typ: String) extends Type {
  require(isJavaPrimitive(this))
  def isArray: Boolean = false
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class ObjectType(val typ: String, val dimensions: Int) extends Type {
  def this(typ: String) = this(typ, 0)
  require(!isJavaPrimitive(this))
  def isArray = dimensions > 0
  def name: String = formatObjectTypeToObjectName(this)
  def simpleName: String = {
    val base = typ.substring(typ.lastIndexOf(".") + 1)
    assign(base, dimensions, "[]", false)
  }
  def canonicalName: String = {
    val base = typ.replaceAll("$", ".")
    assign(base, dimensions, "[]", false)
  }
  def pkg: String = {
    if(isJavaPrimitive(typ) || isArray) null
    else typ.substring(0, typ.lastIndexOf(".")).intern()
  }
  override def toString: String = {
    name
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class NullType() extends Type {
  def typ = "Null"
  def isArray: Boolean = false
  override def toString: String = {
    val sb = new StringBuilder
    sb.append(typ)
    sb.toString.intern()
  }
}

case class InvalidTypeException(msg: String) extends RuntimeException(msg)