/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa.classfile

import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.MethodVisitor
import org.sireum.jawa.AccessFlag
import org.sireum.util._
import org.sireum.jawa.MyMethod
import org.sireum.jawa.MyField
import org.sireum.jawa.MyClass
import org.sireum.jawa.JawaType
import org.sireum.jawa.JavaKnowledge
import org.sireum.jawa.JawaType
import org.sireum.jawa.Signature
import org.sireum.jawa.AccessFlag.FlagKind
import org.sireum.jawa.FieldFQN

/**
 * @author fgwei
 */
class MyClassVisitor(api: Int) extends ClassVisitor(api) {
  private val classes: MMap[JawaType, MyClass] = mmapEmpty
  private var currentClass: MyClass = null
  def getClasses: IMap[JawaType, MyClass] = classes.toMap
  
  private def getClassName(name: String): String = {
    name.replaceAll("/", ".")
  }
  
  override def visit(version: Int, 
            access: Int, 
            name: String,
            signature: String,
            superName: String,
            interfaces: Array[String]): Unit = {
    val accessFlag: Int = AccessFlag.getJawaFlags(access, FlagKind.CLASS, false)
    val typ: JawaType = JavaKnowledge.getTypeFromName(getClassName(name))
    val superType: Option[JawaType] = {
      superName == null match {
        case true => None
        case false => Some(JavaKnowledge.getTypeFromName(getClassName(superName)))
      }
    }
    val ifs: MList[JawaType] = mlistEmpty
    for(interface <- interfaces) {
      ifs += JavaKnowledge.getTypeFromName(getClassName(interface))
    }
    val c = MyClass(accessFlag, typ, superType, ifs.toList)
    classes(typ) = c
    currentClass = c
  }
  
  override def visitOuterClass(owner: String, name: String, desc: String): Unit = {
    val o: JawaType = JavaKnowledge.getTypeFromName(getClassName(name))
    currentClass.setOuter(o)
  }
  
  override def visitField(access: Int, name: String, desc: String,
                 signature: String, value: Object): FieldVisitor = {
    val accessFlag: Int = AccessFlag.getJawaFlags(access, FlagKind.FIELD, false)
    val typ: JawaType = JavaKnowledge.formatSignatureToType(desc)
    val FQN: FieldFQN = FieldFQN(currentClass.typ, name, typ)
    val f = MyField(accessFlag, FQN)
    currentClass.addField(f)
    null
  }
  
  override def visitMethod(access: Int, name: String, desc: String,
                  signature: String, exceptions: Array[String]): MethodVisitor = {
    val accessFlag: Int = AccessFlag.getJawaFlags(access, FlagKind.METHOD, if(name == "<init>" || name == "<clinit>") true else false)
    val signature: Signature = JavaKnowledge.genSignature(JavaKnowledge.formatTypeToSignature(currentClass.typ), name, desc)
    val params = signature.getParameters()
    val paramnames: MList[String] = mlistEmpty
    if(!AccessFlag.isStatic(accessFlag) && !AccessFlag.isAbstract(accessFlag)) paramnames += "this_v"
    for(i <- 0 to params.size - 1){
      paramnames += "v" + i
    }
    val m = MyMethod(accessFlag, signature, paramnames.toList)
    currentClass.addMethod(m)
    new MyMethodVisitor(m)
  }
  
  class MyMethodVisitor(m: MyMethod) extends MethodVisitor(api) {
    override def visitParameter(name: String, access: Int): Unit = {
      m.addParam(name)
    }
  }
}
