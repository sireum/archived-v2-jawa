package org.sireum.jawa.classfile

import org.sireum.jawa.io.AbstractFile
import org.sireum.util._
import org.objectweb.asm.ClassReader
import org.objectweb.asm.Opcodes
import org.sireum.jawa.ObjectType
import org.sireum.jawa.MyClass


/** This object implements a class file parser.
 */
object ClassfileParser {
  def parse(file: AbstractFile): IMap[ObjectType, MyClass] = {
    val cr = new ClassReader(file.toByteArray)
    val mcv = new MyClassVisitor(Opcodes.ASM5)
    cr.accept(mcv, ClassReader.SKIP_CODE)
    mcv.getClasses
  }
}