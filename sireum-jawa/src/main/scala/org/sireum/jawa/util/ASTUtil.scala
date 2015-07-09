/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.util

import org.sireum.pilar.ast._
import org.sireum.util._
import org.sireum.jawa.Signature
import org.sireum.jawa.FieldFQN
import org.sireum.jawa.JawaType
import org.sireum.jawa.JavaKnowledge

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object ASTUtil {
	def getCallArgs(jumploc : JumpLocation) : List[String] = {
	  val argNames : MList[String] = mlistEmpty
    jumploc.jump match {
      case t : CallJump if t.jump.isEmpty =>
        t.callExp.arg match {
          case te : TupleExp =>
            val exps = te.exps
            for(i <- 0 to (exps.size-1)) {
              val varName = exps(i) match{
                case ne : NameExp => ne.name.name
                case a => a.toString()
              }
              argNames += varName
            }
          case _ =>
        }
      case _ =>
    }
	  argNames.toList
	}
  
  def getSignature[T <: Annotable[T]](ast: Annotable[T]): Option[Signature] = {
    ast.getValueAnnotation("signature") match {
      case Some(NameExp(name)) =>
        Some(Signature(name.name))
      case _ => None
    }
  }
  
  def getAccessFlag[T <: Annotable[T]](ast: Annotable[T]): String = {
    ast.getValueAnnotation("AccessFlag") match {
      case Some(NameExp(name)) =>
        name.name
      case _ => ""
    }
  }
  
  def getType[T <: Annotable[T]](ast: Annotable[T]): Option[JawaType] = {
    ast.getValueAnnotation("type") match {
      case Some(TypeExp(typespec)) =>
        Some(getTypeFromTypeSpec(typespec))
      case _ => None
    }
  }
  
  def getTypeFromTypeSpec(ts: TypeSpec): JawaType = {
    var d = 0
    var tmpTs = ts
    while(tmpTs.isInstanceOf[SeqTypeSpec]){
      d += 1
      tmpTs = tmpTs.asInstanceOf[SeqTypeSpec].elementType
    }
    require(tmpTs.isInstanceOf[NamedTypeSpec])
    JawaType.generateType(tmpTs.asInstanceOf[NamedTypeSpec].name.name, d)
  }
  
  def getKind[T <: Annotable[T]](ast: Annotable[T]): String = {
    ast.getValueAnnotation("kind") match {
      case Some(NameExp(name)) =>
        name.name
      case _ => ""
    }
  }
  
  def getFieldFQN(accessExp: AccessExp, typ: JawaType): FieldFQN = {
    new FieldFQN(accessExp.attributeName.name, typ)
  }
}