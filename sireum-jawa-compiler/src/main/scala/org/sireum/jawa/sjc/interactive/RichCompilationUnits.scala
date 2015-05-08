package org.sireum.jawa.sjc.interactive

import org.sireum.util._
import org.sireum.jawa.sjc.parser.CompilationUnit
import org.sireum.jawa.sjc.symtab.CompilationUnitsSymbolTable
import org.sireum.jawa.sjc.symtab.JawaCompilationUnitsSymbolTable

trait RichCompilationUnits { self: Global =>
  private val compilationUnits: MMap[FileResourceUri, CompilationUnit] = mmapEmpty
  private val symbolTable: CompilationUnitsSymbolTable = new JawaCompilationUnitsSymbolTable
  
  def addCompilationUnit(fileUri: FileResourceUri, cu: CompilationUnit) = this.compilationUnits(fileUri) = cu
  def addCompilationUnits(cus: ISeq[CompilationUnit]) = {
    cus.foreach(cu => addCompilationUnit(cu.firstToken.fileUri, cu))
  }
  def removeCompilationUnit(fileUri: FileResourceUri) = this.compilationUnits -= fileUri
  def getCompilationUnits: IMap[FileResourceUri, CompilationUnit] = this.compilationUnits.toMap
  def getCompilationUnit(fileUri: FileResourceUri): Option[CompilationUnit] = this.compilationUnits.get(fileUri)
  def getSymbolTable: CompilationUnitsSymbolTable = this.symbolTable
}