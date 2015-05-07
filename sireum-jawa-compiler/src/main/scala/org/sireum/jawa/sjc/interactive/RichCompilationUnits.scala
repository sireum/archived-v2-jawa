package org.sireum.jawa.sjc.interactive

import org.sireum.util._
import org.sireum.jawa.sjc.parser.CompilationUnit
import org.sireum.jawa.sjc.symtab.JawaCompilationUnitSymbolTable
import org.sireum.jawa.sjc.symtab.CompilationUnitSymbolTable

trait RichCompilationUnits { self: Global =>
  private val compilationUnits: MMap[FileResourceUri, CompilationUnit] = mmapEmpty
  private val symbolTable: CompilationUnitSymbolTable = new JawaCompilationUnitSymbolTable
  
  def addCompilationUnit(fileUri: FileResourceUri, cu: CompilationUnit) = this.compilationUnits(fileUri) = cu
  def addCompilationUnits(cus: ISeq[CompilationUnit]) = {
    cus.foreach{
      cu =>
        cu.firstToken.fileUriOpt match {
          case Some(f) => addCompilationUnit(f, cu)
        }
    }
  }
  def removeCompilationUnit(fileUri: FileResourceUri) = this.compilationUnits -= fileUri
  def getCompilationUnits: IMap[FileResourceUri, CompilationUnit] = this.compilationUnits.toMap
  def getCompilationUnit(fileUri: FileResourceUri): Option[CompilationUnit] = this.compilationUnits.get(fileUri)
  def getSymbolTable: CompilationUnitSymbolTable = this.symbolTable
}