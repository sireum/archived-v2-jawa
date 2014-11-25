/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.reachingFactsAnalysis

import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.Instance
import org.sireum.jawa.Type
import org.sireum.jawa.TupleType
import org.sireum.jawa.NormalType

abstract class RFAAbstractInstance extends Instance

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class RFAInstance(typ : Type, defSite : Context) extends RFAAbstractInstance{
  override def clone(newDefSite : Context) : Instance = RFAInstance(typ, newDefSite)
  override def toString : String = {
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
final case class RFATupleInstance(left : Instance, right : Instance, defSite : Context) extends RFAAbstractInstance{
  override def clone(newDefSite : Context) : Instance = RFATupleInstance(left, right, newDefSite)
  def typ : Type = TupleType(left.typ, right.typ)
  override def toString : String = {
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
abstract class RFAAbstractStringInstance(defSite : Context) extends RFAAbstractInstance{
  def typ : Type = NormalType("java.lang.String", 0) 
  override def toString : String = this.typ + ":abstract@" + this.defSite.getCurrentLocUri
}

/**
 * RFAPointStringInstance represents a general String instance whose content can be any string i.e. reg expression "*"
 * 
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class RFAPointStringInstance(defSite : Context) extends RFAAbstractStringInstance(defSite){
  override def clone(newDefSite : Context) : Instance = RFAPointStringInstance(newDefSite)
  override def toString : String = this.typ + ":point@" + this.defSite.getCurrentLocUri
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class RFAConcreteStringInstance(string : String, defSite : Context) extends RFAAbstractStringInstance(defSite){
  override def clone(newDefSite : Context) : Instance = RFAConcreteStringInstance(string, newDefSite)
  override def toString : String = {
    val sb = new StringBuilder
    sb.append(this.typ + ".")
    sb.append("\"" + {if(this.string.length > 30)this.string.substring(0, 30) + ".." else this.string} + "\".")
    sb.append(this.defSite.getCurrentLocUri)
    sb.toString.intern()
  }
}

