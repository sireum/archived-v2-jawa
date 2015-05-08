package org.sireum.jawa.sjc.symtab

import org.sireum.util._
import org.sireum.jawa.sjc.parser._
import org.sireum.jawa.sjc.parser.{Location => JawaLocation}
import org.sireum.jawa.sjc.lexer.Token
import org.sireum.jawa.sjc.symtab.SymbolTableHelper.CompilationUnitElementMiner
import org.sireum.jawa.sjc.interactive.JawaDelta
import org.sireum.jawa.sjc.interactive.RichCompilationUnits
import org.sireum.jawa.sjc.Signature
import org.sireum.jawa.sjc.ObjectType

trait CompilationUnitsSymbolTable {
  def fileUris: Iterable[FileResourceUri]
  def compilationUnits: Iterable[CompilationUnit]
  def compilationUnit(fileUri: FileResourceUri): CompilationUnit
  def compilationUnitSymbolTables: Iterable[CompilationUnitSymbolTable]
  def compilationUnitSymbolTable(fileUri: FileResourceUri): CompilationUnitSymbolTable
}

trait CompilationUnitSymbolTable {
  def cusSymbolTable: CompilationUnitsSymbolTable
  
  def fileUri: FileResourceUri
  def classOrInterfaceTypes: Iterable[ObjectType]
  def classOrInterfaces: Iterable[ClassOrInterfaceDeclaration]
  def classOrInterface(classOrInterfaceType: ObjectType): ClassOrInterfaceDeclaration
  def classOrInterfaceSymbolTables: Iterable[ClassOrInterfaceSymbolTable]
  def classOrInterfaceSymbolTable(classOrInterfaceType: ObjectType): ClassOrInterfaceSymbolTable 
}

trait ClassOrInterfaceSymbolTable {
  def cuSymbolTable: CompilationUnitSymbolTable
  
  def classOrInterfaceType: ObjectType
  def classOrInterfaceDecl: ClassOrInterfaceDeclaration
  def fieldNames: Iterable[String]
  def fieldDecls: Iterable[Field with Declaration]
  def fieldDecl(fieldSig: String): Field with Declaration
  def staticFieldNames: Iterable[String]
  def staticFieldDecls: Iterable[StaticFieldDeclaration]
  def instanceFieldNames: Iterable[String]
  def instanceFieldDecls: Iterable[InstanceFieldDeclaration]
  def methodNames: Iterable[String]
  def methodSigs: Iterable[Signature]
  def methodDecls: Iterable[MethodDeclaration]
  def methodDecl(methodSig: Signature): MethodDeclaration
  def methodSymbolTables: Iterable[MethodSymbolTable]
  def methodSymbolTable(methodSig: Signature): MethodSymbolTable
}

trait MethodSymbolTable {
  def ciSymbolTable: ClassOrInterfaceSymbolTable

  def methodSig: Signature
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

object JawaCompilationUnitsSymbolTableBuilder {
  def apply[P <: CompilationUnitsSymbolTableProducer](cus: ISeq[CompilationUnit],
            stpConstructor: Unit => P,
            parallel: Boolean) =
    buildSymbolTable(cus, stpConstructor, parallel)

  def apply[P <: CompilationUnitsSymbolTableProducer] //
  (rcu: RichCompilationUnits,
   delta: JawaDelta,
   stpConstructor: Unit => P,
   parallel: Boolean): CompilationUnitsSymbolTable =
    fixSymbolTable(rcu, delta, stpConstructor, parallel)
    
  def apply[P <: CompilationUnitsSymbolTableProducer] //
  (mst: MethodSymbolTable, resolvedBody: ResolvedBody): CompilationUnitsSymbolTable =
    fixSymbolTable(mst, resolvedBody)

  def mineCompilationUnitElements[P <: CompilationUnitsSymbolTableProducer] //
  (cus: ISeq[CompilationUnit], stpConstructor: Unit => P,
   parallel: Boolean): CompilationUnitsSymbolTableProducer = {
    if (cus.isEmpty) return stpConstructor()

    val ms: GenSeq[CompilationUnit] = if (parallel) cus.par else cus
    val stp = stpConstructor()
    ms.foreach { cu =>
      new CompilationUnitElementMiner(stp).mine(cu)
    }
    stp
  }

  def buildSymbolTable(cus: ISeq[CompilationUnit],
                       stpConstructor: Unit => CompilationUnitsSymbolTableProducer,
                       parallel: Boolean) = {
    val stp = mineCompilationUnitElements(cus, stpConstructor, parallel)
    stp.toCompilationUnitsSymbolTable
  }

  def fixSymbolTable //
  (rcu: RichCompilationUnits,
   delta: JawaDelta,
   stpConstructor: Unit => CompilationUnitsSymbolTableProducer,
   parallel: Boolean): CompilationUnitsSymbolTable = {

    delta.changedOrDeletedCUFiles.foreach{
      fileUri =>
        SymbolTableHelper.tearDown(rcu.getSymbolTable.asInstanceOf[CompilationUnitsSymbolTableProducer].tables, fileUri)
    }

    SymbolTableHelper.combine(rcu.getSymbolTable.asInstanceOf[CompilationUnitsSymbolTableProducer], mineCompilationUnitElements(delta.changedOrAddedCUs, stpConstructor, parallel))
    rcu.addCompilationUnits(delta.changedOrAddedCUs.toSeq)
    rcu.getSymbolTable
  }
  
  def fixSymbolTable //
  (mst: MethodSymbolTable, resolvedBody: ResolvedBody): CompilationUnitsSymbolTable = {
    new CompilationUnitElementMiner(mst.ciSymbolTable.cuSymbolTable.cusSymbolTable.asInstanceOf[CompilationUnitsSymbolTableProducer]) 
    .bodySymbol(mst.ciSymbolTable.cuSymbolTable.fileUri, mst.ciSymbolTable.classOrInterfaceType, mst.methodSig, resolvedBody)
    mst.ciSymbolTable.cuSymbolTable.cusSymbolTable
  }
}

trait SymbolTableReporter {
  def reportRedeclaration(fileUri: String, t: Token, template: String, other: JawaAstNode) {
    reportError(fileUri, t, template, other)
  }

  def reportNotFound(fileUri: String, t: Token, template: String) {
    reportError(fileUri, t.line, t.column, t.offset, t.length, template.format(t.text, t.line, t.column))
  }
  
  def reportError(fileUri: String, t: Token, template: String, other: JawaAstNode) {
    reportError(fileUri, t.line, t.column, t.offset, t.length, template.format(t.text, other.firstToken.line, other.firstToken.column))
  }
  
  def reportError(
    fileUri: String, line: Int, column: Int,
    offset: Int, length: Int, message: String): Unit

  def reportWarning(
    fileUri: String, line: Int, column: Int,
    offset: Int, length: Int, message: String): Unit
}

trait CompilationUnitsSymbolTableProducer extends SymbolTableReporter {
  def tables: CompilationUnitsSymbolTableData
  def compilationUnitSymbolTableProducer(fileUri: FileResourceUri): CompilationUnitSymbolTableProducer
  def toCompilationUnitsSymbolTable: CompilationUnitsSymbolTable
}

trait CompilationUnitSymbolTableProducer {
  def tables: CompilationUnitSymbolTableData
  def classOrInterfaceSymbolTableProducer(classOrInterfaceType: ObjectType): ClassOrInterfaceSymbolTableProducer
  def toCompilationUnitSymbolTable: CompilationUnitSymbolTable
}

trait ClassOrInterfaceSymbolTableProducer {
  def tables: ClassOrInterfaceSymbolTableData
  def methodSymbolTableProducer(methodSig: Signature): MethodSymbolTableProducer
  def cuSymbolTableProducer: CompilationUnitSymbolTableProducer
}

trait MethodSymbolTableProducer {
  def tables: MethodSymbolTableData

  def ciSymbolTableProducer: ClassOrInterfaceSymbolTableProducer
}

sealed case class CompilationUnitsSymbolTableData //
(/**
  * Holds the map of file names to their
  * {@link CompilationUnit}.
  */
 compilationUnitTable: MMap[FileResourceUri, CompilationUnit] = mmapEmpty,
 /**
  * Holds the map of file names to their
  * {@link CompilationUnitSymbolTableData}.
  */
 compilationUnitAbsTable: MMap[FileResourceUri, CompilationUnitSymbolTableData] = mmapEmpty)

sealed case class CompilationUnitSymbolTableData //
(/**
  * Holds the map of class fully-qualified names to their
  * {@link ClassOrInterfaceDeclaration}.
  */
 classOrInterfaceTable: MMap[ObjectType, ClassOrInterfaceDeclaration] = mmapEmpty,
 /**
  * Holds the map of class fully-qualified names to their
  * {@link ClassOrInterfaceSymbolTableData}.
  */
 classOrInterfaceAbsTable: MMap[ObjectType, ClassOrInterfaceSymbolTableData] = mmapEmpty)
 
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
 methodTable: MMap[Signature, MethodDeclaration] = mmapEmpty,
 /**
  * Holds the map of method signature to it's
  * {@link JawaMethodSymbolTableData}.
  */
 methodAbsTable: MMap[Signature, MethodSymbolTableData] = mmapEmpty
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