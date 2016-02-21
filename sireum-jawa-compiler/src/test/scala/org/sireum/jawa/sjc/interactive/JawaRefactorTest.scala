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
///*
//Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
//All rights reserved. This program and the accompanying materials      
//are made available under the terms of the Eclipse Public License v1.0 
//which accompanies this distribution, and is available at              
//http://www.eclipse.org/legal/epl-v10.html                             
//*/
//package org.sireum.jawa.sjc.interactive
//
//import org.scalatest._
//import org.sireum.jawa.sjc.lexer.JawaLexer
//import org.sireum.util.FileUtil
//import org.sireum.jawa.io.PlainFile
//import java.io.BufferedReader
//import java.io.StringReader
//import org.sireum.jawa.io.AbstractFile
//import org.sireum.jawa.io.FgSourceFile
//import org.sireum.jawa.DefaultReporter
//import org.sireum.util._
//import org.sireum.jawa.sjc.parser.JawaParser
//import org.sireum.jawa.sjc.parser.CompilationUnit
//import java.io.File
//import java.io.PrintWriter
//import scala.tools.asm.ClassReader
//import scala.tools.asm.util.TraceClassVisitor
//import java.lang.reflect.InvocationTargetException
//import org.sireum.jawa.sjc.refactoring.RefactorJawa
//import org.sireum.jawa.util.MyFileUtil
//import org.sireum.jawa.io.SourceFile
//import org.sireum.pilar.parser.Parser
//import org.sireum.pilar.ast.Model
//
//class JawaRefactorTest extends FlatSpec with ShouldMatchers {
//  
//  "Refactor code" should "not throw an exception on ArrayAccess1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/array/ArrayAccess1.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on ArrayAccess2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/array/ArrayAccess2.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on ArrayAccess3" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/array/ArrayAccess3.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on ArrayCopy" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/array/ArrayCopy.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on ArrayFill1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/array/ArrayFill1.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on ArrayFill2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/array/ArrayFill2.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on ArrayLength1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/array/ArrayLength1.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on Cmp1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/cmp/Cmp1.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on Cmp2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/cmp/Cmp2.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on ConstClass1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/constclass/ConstClass1.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on ConstClass2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/constclass/ConstClass2.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on DoubleLong1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/doublelong/DoubleLong1.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on Exceptions1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/exception/Exceptions1.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on Exceptions2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/exception/Exceptions2.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on Exceptions3" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/exception/Exceptions3.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on Exceptions4" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/exception/Exceptions4.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on FieldAccess1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/field/FieldAccess1.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on FieldAccess2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/field/FieldAccess2.pilar")))
//    refactor(jf)
//  }
//  
////  "Refactor code" should "not throw an exception on StaticFieldAccess1" in {
////    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/field/StaticFieldAccess1.pilar")))
////    refactor(jf)
////  }
//  
//  
//  "Refactor code" should "not throw an exception on Instanceof1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/instance/Instanceof1.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on Instanceof2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/instance/Instanceof2.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on IfJump1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/jump/IfJump1.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on IfJump2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/jump/IfJump2.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on SwitchJump1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/jump/SwitchJump1.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on SwitchJump2" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/jump/SwitchJump2.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on Monitor1" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/monitor/Monitor1.pilar")))
//    refactor(jf)
//  }
//  
//  "Refactor code" should "not throw an exception on a" in {
//    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/other/a.pilar")))
//    refactor(jf)
//  }
//  
////  "Refactor code" should "not throw an exception on FooActivity" in {
////    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/other/FooActivity.pilar")))
////    refactor(jf)
////  }
////  
////  "Refactor code" should "not throw an exception on MainActivity" in {
////    val jf = new FgSourceFile(new PlainFile(new File("src/main/resources/refactoring/other/MainActivity.pilar")))
////    refactor(jf)
////  }
//  
//  val reporter = new DefaultReporter
//  private def parser(s: Either[String, SourceFile]) = new JawaParser(JawaLexer.tokenise(s, reporter).toArray, reporter)
//  private def parseLocation(s: String) = {
//    val loc = parser(Left(s)).location
//    if(reporter.hasErrors) throw new RuntimeException(reporter.problems.toString())
//    loc
//  }
//  
//  private def parseCompilationUnit(s: SourceFile) = {
//    val cu = parser(Right(s)).compilationUnit(true)
//    val allAsts = cu.getAllChildrenInclude
//    allAsts.foreach {
//      ast =>
//        if(!ast.isInstanceOf[CompilationUnit]) require(ast.enclosingTopLevelClass != null, ast + " should have top level class.")
//    }
//    if(reporter.hasErrors) throw new RuntimeException(reporter.problems.toString())
//    cu
//  }
//  
//  private def parseCompilationUnit(s: String) = {
//    val cu = parser(Left(s)).compilationUnit(true)
//    val allAsts = cu.getAllChildrenInclude
//    allAsts.foreach {
//      ast =>
//        if(!ast.isInstanceOf[CompilationUnit]) require(ast.enclosingTopLevelClass != null, ast + " should have top level class.")
//    }
//    if(reporter.hasErrors) throw new RuntimeException(reporter.problems.toString())
//    cu
//  }
//  
//  private def refactor(jf: SourceFile) = {
//    val code = jf.code
////    JawaCodeSource.setPreLoadFlag
////    JawaCodeSource.load(code, GlobalConfig.PILAR_FILE_EXT, AndroidLibraryAPISummary)
////    val code = MyFileUtil.readFileContent(fileUri)
//    val newcode = RefactorJawa(code)
//    println(newcode)
//    JawaParser.parse[CompilationUnit](Left(newcode), true, reporter) // check jawa parser
//    if(reporter.hasErrors) throw new RuntimeException(reporter.problems.toString())    
//    val (model, err) = Parser.parseWithErrorAsString[Model](Left(newcode)) // check pilar parser
//    if(err != "") throw new RuntimeException(err)
//  }
//}
