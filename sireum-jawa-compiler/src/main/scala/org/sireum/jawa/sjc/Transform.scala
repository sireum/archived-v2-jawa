/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc

import org.sireum.util._
import org.sireum.jawa.sjc.symtab.JawaCompilationUnitSymbolTable
import org.sireum.jawa.sjc.parser.CompilationUnit
import org.sireum.jawa.sjc.parser.JawaParser
import org.sireum.jawa.sjc.symtab.JawaCompilationUnitSymbolTableBuilder
import org.sireum.jawa.sjc.symtab.CompilationUnitSymbolTable
import org.sireum.jawa.sjc.parser.ParsableAstNode
import org.sireum.jawa.sjc.interactive.JawaDelta
import org.sireum.jawa.sjc.interactive.RichCompilationUnits
import org.sireum.jawa.sjc.interactive.JawaClass
import org.sireum.jawa.sjc.symtab.MethodSymbolTable
import org.sireum.jawa.sjc.parser.MethodDeclaration

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object Transform {
  import scala.reflect.runtime.{ universe => ru }
	var fst = { _: Unit => new JawaCompilationUnitSymbolTable }
  
	def parseCode[T <: ParsableAstNode : ru.TypeTag](source: (Option[FileResourceUri], String)): T = {
	  val (paopt, err) = JawaParser.parse[T](source._1, source._2) 
	  paopt match{case Some(pa) => pa; case None => throw new RuntimeException(err + "\n" + source._1)}
	}
  
  def getCompilationUnitSymbolResult(rcu: RichCompilationUnits, sources: ISeq[(Option[FileResourceUri], String)]): CompilationUnitSymbolTable = {
    val changedOrDelatedFiles: ISet[FileResourceUri] = sources.filter(_._1.isDefined).map(_._1.get).toSet
    val newCUs: ISeq[CompilationUnit] = sources.map(s => parseCode[CompilationUnit](s))
    val delta: JawaDelta = JawaDelta(changedOrDelatedFiles, newCUs)
    JawaCompilationUnitSymbolTableBuilder(rcu, delta, fst, true)
  }
  
//  def getMethodSymbolResult(rcu: RichCompilationUnits, declaringClass: JawaClass, source: (String, Option[FileResourceUri])): MethodSymbolTable = {
//    val changedOrDelatedFiles: ISet[FileResourceUri] = source._2 match {case Some(f) => Set(f); case None => isetEmpty}
//    val newMd: MethodDeclaration = parseCode[MethodDeclaration](source)
//    val newCU = declaringClass
//  }
}