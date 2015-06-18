package org.sireum.jawa.sjc.util

import java.io.File
import java.io.PrintWriter
import org.sireum.jawa.sjc.codegen.JavaByteCodeGenerator
import org.apache.commons.io.IOUtils
import java.io.FileInputStream

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