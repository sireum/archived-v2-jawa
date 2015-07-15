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
import org.sireum.jawa.sjc.util.ReadClassFile.CustomClassLoader
import org.sireum.jawa.sjc.util.SourceFile

class JawaCodegenTest extends FlatSpec with ShouldMatchers {
  
//  "Generate code" should "not throw an exception on ArrayAccess1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/array/ArrayAccess1.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on ArrayAccess2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/array/ArrayAccess2.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on ArrayAccess3" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/array/ArrayAccess3.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on ArrayCopy" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/array/ArrayCopy.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on ArrayFill1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/array/ArrayFill1.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on ArrayFill2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/array/ArrayFill2.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on ArrayLength1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/array/ArrayLength1.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on Cmp1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/cmp/Cmp1.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on Cmp2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/cmp/Cmp2.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on ConstClass1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/constclass/ConstClass1.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on ConstClass2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/constclass/ConstClass2.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on DoubleLong1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/doublelong/DoubleLong1.pilar")))
//    genCode(jf)
//  }
//  
//  "Generate code" should "not throw an exception on Exceptions1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/exception/Exceptions1.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on Exceptions2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/exception/Exceptions2.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on Exceptions3" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/exception/Exceptions3.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "throw an exception on Exceptions4" in {
//    evaluating {
//      val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/exception/Exceptions4.pilar")))
//      genCode(jf.file)
//    } should produce[RuntimeException]
//  }
//  
//  "Generate code" should "not throw an exception on FieldAccess1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/field/FieldAccess1.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on FieldAccess2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/field/FieldAccess2.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on StaticFieldAccess1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/field/StaticFieldAccess1.pilar")))
//    genCode(jf.file)
//  }
//  
//  
//  "Generate code" should "not throw an exception on Instanceof1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/instance/Instanceof1.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on Instanceof2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/instance/Instanceof2.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on IfJump1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/jump/IfJump1.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on IfJump2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/jump/IfJump2.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on SwitchJump1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/jump/SwitchJump1.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on SwitchJump2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/jump/SwitchJump2.pilar")))
//    genCode(jf.file)
//  }
//  
//  "Generate code" should "not throw an exception on Monitor1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/monitor/Monitor1.pilar")))
//    genCode(jf.file)
//  }
//  
  "Generate code" should "not throw an exception on Other" in {
    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/codegen/other/ArrayAccess1.pilar")))
    genCode(jf)
  }
  
  val reporter = new DefaultReporter
  private def parser(s: Either[String, SourceFile]) = new JawaParser(JawaLexer.tokenise(s, reporter).toArray, reporter)
  private def parseLocation(s: String) = {
    val loc = parser(Left(s)).location
    if(reporter.hasErrors) throw new RuntimeException(reporter.problems.toString())
    loc
  }
  
  private def parseCompilationUnit(s: SourceFile) = {
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
  
  private def genCode(s: SourceFile) = {    
    val cu = parser(Right(s)).compilationUnit(true)
    val css = new JavaByteCodeGenerator().generate(cu)
    val ccl: CustomClassLoader = new CustomClassLoader()
    val pw = new PrintWriter(System.out)
    css foreach {
      case (typ, bytecodes) => 
        JavaByteCodeGenerator.outputByteCodes(pw, bytecodes)
        try{
          val c = ccl.loadClass(typ.name, bytecodes)
          val r = c.getMethod("main").invoke(null)
          println("result: " + r)
        } catch {
          case ite: InvocationTargetException =>
            throw ite.getTargetException
        }
    }
  }
}