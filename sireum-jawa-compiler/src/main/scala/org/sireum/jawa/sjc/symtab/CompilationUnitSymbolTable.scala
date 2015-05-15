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
import org.sireum.jawa.sjc.io.AbstractFile
import org.sireum.jawa.sjc.ReporterImpl
import org.sireum.jawa.sjc.util.Position
import org.sireum.jawa.sjc.interactive.Problem

trait CompilationUnitsSymbolTable {
  def files: Iterable[AbstractFile]
  def compilationUnits: Iterable[CompilationUnit]
  def compilationUnit(file: AbstractFile): CompilationUnit
  def compilationUnitSymbolTables: Iterable[CompilationUnitSymbolTable]
  def compilationUnitSymbolTable(file: AbstractFile): CompilationUnitSymbolTable
}

trait CompilationUnitSymbolTable {
  def cusSymbolTable: CompilationUnitsSymbolTable
  
  def file: AbstractFile
  def unit: CompilationUnit
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
  (rcus: RichCompilationUnits,
   delta: JawaDelta,
   stpConstructor: Unit => CompilationUnitsSymbolTableProducer,
   parallel: Boolean): CompilationUnitsSymbolTable = {

    delta.changedOrDeletedCUFiles.foreach{
      file =>
        SymbolTableHelper.tearDown(rcus.getSymbolTable.asInstanceOf[CompilationUnitsSymbolTableProducer].tables, file)
    }

    SymbolTableHelper.combine(rcus.getSymbolTable.asInstanceOf[CompilationUnitsSymbolTableProducer], mineCompilationUnitElements(delta.changedOrAddedCUs, stpConstructor, parallel))
    delta.changedOrAddedCUs.foreach{
      cu =>
        val file = cu.firstToken.file.file
        val problems: ISet[Problem] = rcus.getSymbolTable.problems.getOrElse(file, msetEmpty).toSet
        val rcu = rcus.RichCompilationUnit(cu, rcus.getSymbolTable.compilationUnitSymbolTable(file))
        rcu.problems ++= problems
        rcus.addCompilationUnit(file, rcu)
    }
    rcus.getSymbolTable
  }
  
  def fixSymbolTable //
  (mst: MethodSymbolTable, resolvedBody: ResolvedBody): CompilationUnitsSymbolTable = {
    new CompilationUnitElementMiner(mst.ciSymbolTable.cuSymbolTable.cusSymbolTable.asInstanceOf[CompilationUnitsSymbolTableProducer]) 
    .bodySymbol(mst.ciSymbolTable.cuSymbolTable.file, mst.ciSymbolTable.classOrInterfaceType, mst.methodSig, resolvedBody)
    mst.ciSymbolTable.cuSymbolTable.cusSymbolTable
  }
}

trait SymbolTableReporter extends ReporterImpl {
  def reportRedeclaration(fileUri: String, t: Token, template: String, other: JawaAstNode) {
    reportError(fileUri, t, template, other)
  }

  def reportNotFound(fileUri: String, t: Token, template: String) {
    error(t.pos, template.format(t.text, t.line, t.column))
  }
  
  def reportError(fileUri: String, t: Token, template: String, other: JawaAstNode) {
    error(t.pos, template.format(t.text, other.firstToken.line, other.firstToken.column))
  }
  
}

trait CompilationUnitsSymbolTableProducer extends SymbolTableReporter {
  def tables: CompilationUnitsSymbolTableData
  def compilationUnitSymbolTableProducer(file: AbstractFile): CompilationUnitSymbolTableProducer
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
 compilationUnitTable: MMap[AbstractFile, CompilationUnit] = mmapEmpty,
 /**
  * Holds the map of file names to their
  * {@link CompilationUnitSymbolTableData}.
  */
 compilationUnitAbsTable: MMap[AbstractFile, CompilationUnitSymbolTableData] = mmapEmpty)

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