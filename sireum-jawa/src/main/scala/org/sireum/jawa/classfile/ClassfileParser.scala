/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.jawa.classfile

import org.sireum.jawa.io.AbstractFile
import org.sireum.util._
import org.objectweb.asm.ClassReader
import org.objectweb.asm.Opcodes
import org.sireum.jawa.JawaType
import org.sireum.jawa.MyClass


/** This object implements a class file parser.
 */
object ClassfileParser {
  def parse(file: AbstractFile): IMap[JawaType, MyClass] = {
    val cr = new ClassReader(file.toByteArray)
    val mcv = new MyClassVisitor(Opcodes.ASM5)
    cr.accept(mcv, ClassReader.SKIP_CODE)
    mcv.getClasses
  }
}
