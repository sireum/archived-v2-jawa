package org.sireum.jawa.sjc.interactive

import org.sireum.util._
import org.sireum.jawa.sjc.parser.CompilationUnit
import org.sireum.jawa.sjc.symtab.JawaCompilationUnitSymbolTable
import org.sireum.jawa.sjc.symtab.CompilationUnitSymbolTable

trait RichCompilationUnits { self: Global =>
  private val fileCompilationUnits: MMap[FileResourceUri, CompilationUnit] = mmapEmpty
  private val tempCompilationUnits: MSet[CompilationUnit] = msetEmpty
  private val symbolTable: CompilationUnitSymbolTable = new JawaCompilationUnitSymbolTable
  
  def addCompilationUnit(fileUri: FileResourceUri, cu: CompilationUnit) = this.fileCompilationUnits(fileUri) = cu
  def addCompilationUnits(cus: ISeq[CompilationUnit]) = {
    cus.foreach{
      cu =>
        cu.firstToken.fileUriOpt match {
          case Some(f) => addCompilationUnit(f, cu)
          case None => addTempCompilationUnits(List(cu))
        }
    }
  }
  def removeCompilationUnit(fileUri: FileResourceUri) = this.fileCompilationUnits -= fileUri
  def getCompilationUnits: IMap[FileResourceUri, CompilationUnit] = this.fileCompilationUnits.toMap
  def getCompilationUnit(fileUri: FileResourceUri): Option[CompilationUnit] = this.fileCompilationUnits.get(fileUri)
  def addTempCompilationUnits(cus: ISeq[CompilationUnit]) = this.tempCompilationUnits ++= cus
  def clearTempCompilationUnits = this.tempCompilationUnits.clear()
  def getSymbolTable: CompilationUnitSymbolTable = this.symbolTable
}