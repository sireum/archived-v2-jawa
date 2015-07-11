package org.sireum.jawa.classfile

import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.MethodVisitor
import org.sireum.jawa.AccessFlag
import org.sireum.util._
import org.sireum.jawa.MyMethod
import org.sireum.jawa.MyField
import org.sireum.jawa.MyClass
import org.sireum.jawa.ObjectType
import org.sireum.jawa.JavaKnowledge
import org.sireum.jawa.JawaType
import org.sireum.jawa.Signature
import org.sireum.jawa.AccessFlag.FlagKind
import org.sireum.jawa.FieldFQN

/**
 * @author fgwei
 */
class MyClassVisitor(api: Int) extends ClassVisitor(api) {
  private val classes: MMap[ObjectType, MyClass] = mmapEmpty
  private var currentClass: MyClass = null
  def getClasses: IMap[ObjectType, MyClass] = classes.toMap
  
  private def getClassName(name: String): String = {
    name.replaceAll("/", ".")
  }
  
  override def visit(version: Int, 
            access: Int, 
            name: String,
            signature: String,
            superName: String,
            interfaces: Array[String]): Unit = {
    val accessFlag: Int = AccessFlag.getJawaFlags(access, FlagKind.CLASS)
    val typ: ObjectType = JavaKnowledge.getTypeFromName(getClassName(name)).asInstanceOf[ObjectType]
    val superType: Option[ObjectType] = {
      superName == null match {
        case true => None
        case false => Some(JavaKnowledge.getTypeFromName(getClassName(superName)).asInstanceOf[ObjectType])
      }
    }
    val ifs: MList[ObjectType] = mlistEmpty
    for(interface <- interfaces) {
      ifs += JavaKnowledge.getTypeFromName(getClassName(superName)).asInstanceOf[ObjectType]
    }
    val c = MyClass(accessFlag, typ, superType, ifs.toList)
    classes(typ) = c
    currentClass = c
  }
  
  override def visitOuterClass(owner: String, name: String, desc: String): Unit = {
    val o: ObjectType = JavaKnowledge.getTypeFromName(getClassName(name)).asInstanceOf[ObjectType]
    currentClass.setOuter(o)
  }
  
  override def visitField(access: Int, name: String, desc: String,
                 signature: String, value: Object): FieldVisitor = {
    val accessFlag: Int = AccessFlag.getJawaFlags(access, FlagKind.FIELD)
    val typ: JawaType = JavaKnowledge.formatSignatureToType(desc)
    val FQN: FieldFQN = FieldFQN(currentClass.typ, name, typ)
    val f = MyField(accessFlag, FQN)
    currentClass.addField(f)
    null
  }
  
  override def visitMethod(access: Int, name: String, desc: String,
                  signature: String, exceptions: Array[String]): MethodVisitor = {
    val accessFlag: Int = AccessFlag.getJawaFlags(access, FlagKind.METHOD)
    val signature: Signature = JavaKnowledge.genSignature(JavaKnowledge.formatTypeToSignature(currentClass.typ), name, desc)
    val params = signature.getParameters()
    val paramnames: IList[String] = params.map {
      param => "v" + params.indexOf(param)
    }
    val m = MyMethod(accessFlag, signature, paramnames)
    currentClass.addMethod(m)
    new MyMethodVisitor(m)
  }
  
  class MyMethodVisitor(m: MyMethod) extends MethodVisitor(api) {
    override def visitParameter(name: String, access: Int): Unit = {
      m.addParam(name)
    }
  }
}