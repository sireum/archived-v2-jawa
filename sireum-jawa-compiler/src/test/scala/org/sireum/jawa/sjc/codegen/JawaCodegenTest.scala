/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.codegen

import org.scalatest._
import org.sireum.jawa.sjc.lexer.JawaLexer
import org.sireum.util.FileUtil
import org.sireum.jawa.sjc.io.PlainFile
import java.io.BufferedReader
import java.io.StringReader
import org.sireum.jawa.sjc.io.AbstractFile
import org.sireum.jawa.sjc.util.FgSourceFile
import org.sireum.jawa.sjc.DefaultReporter
import org.sireum.util._
import org.sireum.jawa.sjc.parser.JawaParser
import org.sireum.jawa.sjc.parser.CompilationUnit
import java.io.File
import java.io.PrintWriter
import scala.tools.asm.ClassReader
import scala.tools.asm.util.TraceClassVisitor
import java.lang.reflect.InvocationTargetException

class JawaCodegenTest extends FlatSpec with ShouldMatchers {
  
  "Generate code" should "not throw an exception on ArrayAccess1" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/array/ArrayAccess1.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on ArrayAccess2" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/array/ArrayAccess2.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on ArrayAccess3" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/array/ArrayAccess3.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on ArrayCopy" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/array/ArrayCopy.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on ArrayFill1" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/array/ArrayFill1.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on ArrayFill2" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/array/ArrayFill2.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on ArrayLength1" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/array/ArrayLength1.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on Cmp1" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/cmp/Cmp1.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on Cmp2" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/cmp/Cmp2.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on ConstClass1" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/constclass/ConstClass1.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on ConstClass2" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/constclass/ConstClass2.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on FieldAccess1" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/field/FieldAccess1.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on FieldAccess2" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/field/FieldAccess2.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on Instanceof1" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/instance/Instanceof1.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on Instanceof2" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/instance/Instanceof2.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on Exceptions1" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/exception/Exceptions1.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on Exceptions2" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/exception/Exceptions2.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on Exceptions3" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/exception/Exceptions3.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "throw an exception on Exceptions4" in {
    evaluating {
      val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/exception/Exceptions4.pilar")))
      genCode(jf.file)
    } should produce[RuntimeException]
  }
  
  "Generate code" should "not throw an exception on IfJump1" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/jump/IfJump1.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on IfJump2" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/jump/IfJump2.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on SwitchJump1" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/jump/SwitchJump1.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on SwitchJump2" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/jump/SwitchJump2.pilar")))
    genCode(jf.file)
  }
  
  "Generate code" should "not throw an exception on Monitor1" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/monitor/Monitor1.pilar")))
    genCode(jf.file)
  }
  
  val reporter = new DefaultReporter
  private def parser(s: Either[String, AbstractFile]) = new JawaParser(JawaLexer.tokenise(s, reporter).toArray, reporter)
  private def parseLocation(s: String) = {
    val loc = parser(Left(s)).location
    if(reporter.hasErrors) throw new RuntimeException(reporter.problems.toString())
    loc
  }
  
  private def parseCompilationUnit(s: AbstractFile) = {
    val cu = parser(Right(s)).compilationUnit(true)
    val allAsts = cu.getAllChildrenInclude
    allAsts.foreach {
      ast =>
        if(!ast.isInstanceOf[CompilationUnit]) require(ast.enclosingTopLevelClass != null, ast + " should have top level class.")
    }
    if(reporter.hasErrors) throw new RuntimeException(reporter.problems.toString())
    cu
  }
  
  private def parseCompilationUnit(s: String) = {
    val cu = parser(Left(s)).compilationUnit(true)
    val allAsts = cu.getAllChildrenInclude
    allAsts.foreach {
      ast =>
        if(!ast.isInstanceOf[CompilationUnit]) require(ast.enclosingTopLevelClass != null, ast + " should have top level class.")
    }
    if(reporter.hasErrors) throw new RuntimeException(reporter.problems.toString())
    cu
  }
  
  private def genCode(s: AbstractFile) = {    
    val cu = parser(Right(s)).compilationUnit(true)
    val css = new JavaByteCodeGenerator().generate(cu)
    val ccl: CustomClassLoader = new CustomClassLoader()
    css foreach {
      case (name, bytecodes) => 
        val pw = new PrintWriter(System.out)
        JavaByteCodeGenerator.outputByteCodes(pw, bytecodes)
        try{
          val c = ccl.loadClass(name, bytecodes)
          val r = c.getMethod("main").invoke(null)
          println("result: " + r)
        } catch {
          case ite: InvocationTargetException =>
            throw ite.getTargetException
        }
    }
  }

  class CustomClassLoader extends ClassLoader {
    def loadClass(name: String, bytecodes: Array[Byte]): Class[_ <: Any] = {
      return defineClass(name, bytecodes, 0, bytecodes.length)
    }
  }
}