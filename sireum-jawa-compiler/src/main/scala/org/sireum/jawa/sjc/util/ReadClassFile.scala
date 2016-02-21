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
package org.sireum.jawa.sjc.util

import java.io.File
import java.io.PrintWriter
import org.sireum.jawa.sjc.codegen.JavaByteCodeGenerator
import java.io.FileInputStream
import org.apache.tika.io.IOUtils

/**
 * @author fgwei
 */
object ReadClassFile {
  
  def main(args: Array[String]): Unit = {
    val file: File = new File(args(0))
    read(file)
  }
  
  class CustomClassLoader extends ClassLoader {
    def loadClass(name: String, bytecodes: Array[Byte]): Class[_ <: Any] = {
      return defineClass(name, bytecodes, 0, bytecodes.length)
    }
  }
  
  def read(file: File): Unit = {
    val bytecodes = IOUtils.toByteArray(new FileInputStream(file))
    val ccl: CustomClassLoader = new CustomClassLoader()
    val pw = new PrintWriter(System.out)
    JavaByteCodeGenerator.outputByteCodes(pw, bytecodes)
//    println("result: " + r)
  }
}
