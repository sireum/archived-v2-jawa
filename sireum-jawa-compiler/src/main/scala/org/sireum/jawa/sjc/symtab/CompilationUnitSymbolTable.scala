package org.sireum.jawa.sjc.symtab

import org.sireum.util._
import org.sireum.jawa.sjc.parser._
import org.sireum.jawa.sjc.parser.{Location => JawaLocation}
import org.sireum.jawa.sjc.lexer.Token
import org.sireum.jawa.sjc.symtab.SymbolTableHelper.CompilationUnitElementMiner
import org.sireum.jawa.sjc.interactive.JawaDelta
import org.sireum.jawa.sjc.interactive.RichCompilationUnits

trait CompilationUnitsSymbolTable {
  def compilationUnits: Iterable[CompilationUnit]
  def fileCompilationUnit(fileUri: FileResourceUri): CompilationUnit
  def tempCompilationUnits: Iterable[CompilationUnit]
  def compilationUnitSymbolTables: Iterable[CompilationUnitSymbolTable]
  def fileCompilationUnitSymbolTable(fileUri: FileResourceUri): CompilationUnitSymbolTable
  def tempCompilationUnitSymbolTables: Iterable[CompilationUnitSymbolTable]
}

trait CompilationUnitSymbolTable {
  def classOrInterfaceNames: Iterable[String]
  def classOrInterfaces: Iterable[ClassOrInterfaceDeclaration]
  def classOrInterface(classOrInterfaceName: String): ClassOrInterfaceDeclaration
  def classOrInterfaceSymbolTables: Iterable[ClassOrInterfaceSymbolTable]
  def classOrInterfaceSymbolTable(classOrInterfaceName: String): ClassOrInterfaceSymbolTable 
}

trait ClassOrInterfaceSymbolTable {
  def cuSymbolTable: CompilationUnitSymbolTable
  
  def classOrInterfaceName: String
  def classOrInterfaceDecl: ClassOrInterfaceDeclaration
  def fieldNames: Iterable[String]
  def fieldDecls: Iterable[Field with Declaration]
  def fieldDecl(fieldSig: String): Field with Declaration
  def staticFieldNames: Iterable[String]
  def staticFieldDecls: Iterable[StaticFieldDeclaration]
  def instanceFieldNames: Iterable[String]
  def instanceFieldDecls: Iterable[InstanceFieldDeclaration]
  def methodNames: Iterable[String]
  def methodSigs: Iterable[String]
  def methodDecls: Iterable[MethodDeclaration]
  def methodDecl(methodSig: String): MethodDeclaration
  def methodSymbolTables: Iterable[MethodSymbolTable]
  def methodSymbolTable(methodSig: String): MethodSymbolTable
}

trait MethodSymbolTable {
  def ciSymbolTable: ClassOrInterfaceSymbolTable

  def methodSig: String
  def methodName: String
  def methodDecl: MethodDeclaration
  def thisNameOpt: Option[String]
  def thisOpt: Option[Param]
  def paramNames: ISeq[String]
  def params: ISeq[Param]
  def isParam(varname: String): Boolean
  def param(paramName: String): Param
  def localNames: Iterable[String]
  def locals: Iterable[LocalVarDeclaration]
  def local(localName: String): LocalVarDeclaration
  def locations: ISeq[JawaLocation]
  def location(locationUri: String): JawaLocation
  def location(locationIndex: Int): JawaLocation
  def catchClauses(locationIndex: Int): Iterable[CatchClause]
}

object JawaCompilationUnitSymbolTableBuilder {
  def apply[P <: CompilationUnitSymbolTableProducer](cus: ISeq[CompilationUnit],
            stpConstructor: Unit => P,
            parallel: Boolean) =
    buildSymbolTable(cus, stpConstructor, parallel)

  def apply[P <: CompilationUnitSymbolTableProducer] //
  (rcu: RichCompilationUnits,
   delta: JawaDelta,
   stpConstructor: Unit => P,
   parallel: Boolean): CompilationUnitSymbolTable =
    fixSymbolTable(rcu, delta, stpConstructor, parallel)

  def mineCompilationUnitElements[P <: CompilationUnitSymbolTableProducer] //
  (cus: ISeq[CompilationUnit], stpConstructor: Unit => P,
   parallel: Boolean): CompilationUnitSymbolTableProducer = {
    if (cus.isEmpty) return stpConstructor()

    val ms: GenSeq[CompilationUnit] = if (parallel) cus.par else cus
    ms.map { cu =>
      val stp = stpConstructor()
      new CompilationUnitElementMiner(stp).mine(cu)
      val tables = stp.tables
      cu.firstToken.fileUriOpt.foreach { fileUri =>
        val set = msetEmpty[ResourceUri]
        set ++= tables.classOrInterfaceTable.keys
        tables.declaredSymbols(fileUri) = set
      }
      stp
    }.toIterable.reduce(SymbolTableHelper.combine)
  }

  def buildSymbolTable(cus: ISeq[CompilationUnit],
                       stpConstructor: Unit => CompilationUnitSymbolTableProducer,
                       parallel: Boolean) = {
    val stp = mineCompilationUnitElements(cus, stpConstructor, parallel)
    stp.toCompilationUnitSymbolTable
  }

  def fixSymbolTable //
  (rcu: RichCompilationUnits,
   delta: JawaDelta,
   stpConstructor: Unit => CompilationUnitSymbolTableProducer,
   parallel: Boolean): CompilationUnitSymbolTable = {

    delta.changedOrDeletedCUFiles.foreach{
      fileUri =>
        val cu = rcu.getCompilationUnits(fileUri)
        SymbolTableHelper.tearDown(rcu.getSymbolTable.asInstanceOf[CompilationUnitSymbolTableProducer].tables, cu)
    }

    SymbolTableHelper.combine(rcu.getSymbolTable.asInstanceOf[CompilationUnitSymbolTableProducer], mineCompilationUnitElements(delta.changedOrAddedCUs, stpConstructor, parallel))
    rcu.addCompilationUnits(delta.changedOrAddedCUs.toSeq)
    rcu.getSymbolTable
  }
}

trait SymbolTableReporter {
  def reportRedeclaration(fileUri: Option[String], t: Token, template: String, other: JawaAstNode) {
    reportError(fileUri, t, template, other)
  }

  def reportNotFound(fileUri: Option[String], t: Token, template: String) {
    reportError(fileUri, t.line, t.column, t.offset, t.length, template.format(t.text, t.line, t.column))
  }
  
  def reportError(fileUri: Option[String], t: Token, template: String, other: JawaAstNode) {
    reportError(fileUri, t.line, t.column, t.offset, t.length, template.format(t.text, other.firstToken.line, other.firstToken.column))
  }
  
  def reportError(
    fileUri: Option[String], line: Int, column: Int,
    offset: Int, length: Int, message: String): Unit

  def reportWarning(
    fileUri: Option[String], line: Int, column: Int,
    offset: Int, length: Int, message: String): Unit
}

trait CompilationUnitsSymbolTableProducer extends SymbolTableReporter {
  def fileCompilationUnitSymbolTableProducer(fileUri: FileResourceUri): CompilationUnitSymbolTableProducer
  def toCompilationUnitsSymbolTable: CompilationUnitsSymbolTable
}

trait CompilationUnitSymbolTableProducer extends SymbolTableReporter {
  def tables: CompilationUnitSymbolTableData
  def classOrInterfaceSymbolTableProducer(classOrInterfaceName: String): ClassOrInterfaceSymbolTableProducer
  def toCompilationUnitSymbolTable: CompilationUnitSymbolTable
}

trait ClassOrInterfaceSymbolTableProducer {
  def tables: ClassOrInterfaceSymbolTableData
  def methodSymbolTableProducer(methodSig: String): MethodSymbolTableProducer
  def cuSymbolTableProducer: CompilationUnitSymbolTableProducer
}

trait MethodSymbolTableProducer {
  def tables: MethodSymbolTableData

  def ciSymbolTableProducer: ClassOrInterfaceSymbolTableProducer
}

sealed case class CompilationUnitSymbolTableData //
(declaredSymbols: MMap[FileResourceUri, MSet[ResourceUri]] = mmapEmpty,
 /**
  * Holds the map of class fully-qualified names to their
  * {@link TypeDeclaration}.
  */
 classOrInterfaceTable: MMap[String, ClassOrInterfaceDeclaration] = mmapEmpty,
 /**
  * Holds the map of class fully-qualified names to their
  * {@link classOrInterfaceAbsTable}.
  */
 classOrInterfaceAbsTable: MMap[String, ClassOrInterfaceSymbolTableData] = mmapEmpty)
 
sealed case class ClassOrInterfaceSymbolTableData
(/**
  * Holds the map of field signature to their
  * {@link Field} with {@link Declaration}.
  */
 fieldTable: MMap[String, Field with Declaration] = mmapEmpty,
 /**
  * Holds the map of method signature to it's
  * {@link MethodDeclaration}.
  */
 methodTable: MMap[String, MethodDeclaration] = mmapEmpty,
 /**
  * Holds the map of method signature to it's
  * {@link JawaMethodSymbolTableData}.
  */
 methodAbsTable: MMap[String, MethodSymbolTableData] = mmapEmpty
 )
 
sealed case class MethodSymbolTableData //
(/**
  * Holds the map of local variable name to it's
  * {@link LocalVarDeclaration}.
  */
 localVarTable: MMap[String, LocalVarDeclaration] = mmapEmpty,
 /**
  * Holds the list of {@link Param}.
  */
 params: MList[Param] = mlistEmpty,
 /**
  * Holds the {@link JawaMethodBodySymbolTableData}.
  */
 var bodyTables: Option[MethodBodySymbolTableData] = None)

sealed case class MethodBodySymbolTableData //
(/**
  * Holds the map of location uri to it's
  * {@link JawaLocation}.
  */
 locationTable: MMap[String, JawaLocation] = mlinkedMapEmpty,
 /**
  * Holds the list of {@link CatchClause}.
  */
 catchTable: MMap[Int, MBuffer[CatchClause]] = mmapEmpty)