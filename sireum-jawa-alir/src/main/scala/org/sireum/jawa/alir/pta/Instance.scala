/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.pta

import org.sireum.jawa.Type
import org.sireum.jawa.TupleType
import org.sireum.jawa.NormalType
import org.sireum.util._
import org.sireum.jawa.NullType
import org.sireum.jawa.alir.Context

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
abstract class Instance{
  def typ: Type
  def getType = typ
  def defSite: Context
  def getDefSite = defSite
  def ===(ins: Instance): Boolean = this == ins
  def clone(newDefSite: Context): Instance
  private val fieldsUnknownDefSites: MMap[Context, Set[String]] = mmapEmpty
  def addFieldsUnknownDefSite(defSite: Context, fields: Set[String]) = this.fieldsUnknownDefSites += (defSite -> fields)
  def setFieldsUnknownDefSites(defSites: IMap[Context, Set[String]]) = {
    this.fieldsUnknownDefSites.clear()
    this.fieldsUnknownDefSites ++= defSites
  }
  def getFieldsUnknownDefSites: IMap[Context, ISet[String]] = this.fieldsUnknownDefSites.toMap
}


final case class ClassInstance(name: String, defSite: Context) extends Instance{
  override def clone(newDefSite: Context): Instance = ClassInstance(name, newDefSite)
  def typ = NormalType("java.lang.Class", 0)
  def getName = name
  override def ===(ins: Instance): Boolean = {
    if(ins.isInstanceOf[ClassInstance]) ins.asInstanceOf[ClassInstance].getName.equals(getName)
    else false
  }
  override def toString: String = this.name + ".class@" + this.defSite.getCurrentLocUri
}

final case class NullInstance(defSite: Context) extends Instance{
  override def clone(newDefSite: Context): Instance = NullInstance(newDefSite)
  def typ: Type = new NullType
  override def toString: String = "Null" + "@" + defSite.getCurrentLocUri
}

final case class UnknownInstance(baseTyp: Type, defSite: Context) extends Instance{
  override def clone(newDefSite: Context): Instance = UnknownInstance(baseTyp, newDefSite)
  def typ: Type = baseTyp
  override def toString: String = baseTyp + "*" + "@" + defSite.getCurrentLocUri
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class PTAInstance(typ: Type, defSite: Context) extends Instance{
  override def clone(newDefSite: Context): Instance = PTAInstance(typ, newDefSite)
  override def toString: String = {
    val sb = new StringBuilder
    sb.append(this.typ + "@")
    sb.append(this.defSite.getCurrentLocUri)
    sb.toString.intern()
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class PTATupleInstance(left: Instance, right: Instance, defSite: Context) extends Instance{
  override def clone(newDefSite: Context): Instance = PTATupleInstance(left, right, newDefSite)
  def typ: Type = TupleType(left.typ, right.typ)
  override def toString: String = {
    val sb = new StringBuilder
    sb.append(this.typ + "@")
    sb.append(this.defSite.getCurrentLocUri)
    sb.toString.intern()
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
abstract class PTAAbstractStringInstance(defSite: Context) extends Instance{
  def typ: Type = NormalType("java.lang.String", 0) 
  override def toString: String = this.typ + ":abstract@" + this.defSite.getCurrentLocUri
}

/**
 * PTAPointStringInstance represents a general String instance whose content can be any string i.e. reg expression "*"
 * 
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class PTAPointStringInstance(defSite: Context) extends PTAAbstractStringInstance(defSite){
  override def clone(newDefSite: Context): Instance = PTAPointStringInstance(newDefSite)
  override def ===(ins: Instance): Boolean = {
    if(ins.isInstanceOf[PTAPointStringInstance]) true
    else if(ins.isInstanceOf[PTAConcreteStringInstance]) true
    else false
  }
  override def toString: String = this.typ + ":*@" + this.defSite.getCurrentLocUri
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class PTAConcreteStringInstance(string: String, defSite: Context) extends PTAAbstractStringInstance(defSite){
  override def clone(newDefSite: Context): Instance = PTAConcreteStringInstance(string, newDefSite)
  override def ===(ins: Instance): Boolean = {
    if(ins.isInstanceOf[PTAConcreteStringInstance]) ins.asInstanceOf[PTAConcreteStringInstance].string.equals(string)
    else if(ins.isInstanceOf[PTAPointStringInstance]) true
    else false
  }
  override def toString: String = {
    val sb = new StringBuilder
    sb.append(this.typ + ":")
    sb.append("\"" + {if(this.string.length > 30)this.string.substring(0, 30) + ".." else this.string} + "\"@")
    sb.append(this.defSite.getCurrentLocUri)
    sb.toString.intern()
  }
}

