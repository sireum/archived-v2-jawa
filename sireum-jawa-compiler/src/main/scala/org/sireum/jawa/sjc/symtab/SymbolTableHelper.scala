package org.sireum.jawa.sjc.symtab

import org.sireum.util._
import org.sireum.jawa.sjc.parser._
import org.sireum.jawa.sjc.parser.{Location => JawaLocation}
import org.sireum.jawa.sjc.lexer.Token
import org.sireum.jawa.sjc.ObjectType
import org.sireum.jawa.sjc.Signature

object SymbolTableHelper {
  val DUPLICATE_CU = // 
    "CompilationUnit '%s' has already been defined at [%d, %d]"
  val DUPLICATE_METHOD = // 
    "Method '%s' with same signature has already been defined at [%d, %d]"

  val DUPLICATE_CLASS = // 
    "Class '%s' has already been defined at [%d, %d]"

  val DUPLICATE_FIELD = // 
    "Field '%s' with same signature has already been defined at [%d, %d]"

  val DUPLICATE_PARAM = // 
    "Parameter '%s' has already been defined at [%d, %d]"
  
  val DUPLICATE_LOCAL_VAR = // 
    "Local variable '%s' has already been defined at [%d, %d]"
  
  val DUPLICATE_LOCATION = // 
    "Location '%s' has already been defined at [%d, %d]"
  
  val CATCH_TABLE_END_BEFORE_START = //
    "Ill-formed catch clause due to region end '%s' appear before '%s' at [%d, %d]"
  
  val OF_FILE = " of %s"
  
  abstract class StpWrapper(stp: CompilationUnitsSymbolTableProducer) extends CompilationUnitsSymbolTableProducer {

    def tables: CompilationUnitsSymbolTableData = stp.tables

    def compilationUnitSymbolTableProducer
    (fileUri: FileResourceUri): CompilationUnitSymbolTableProducer =
      stp.compilationUnitSymbolTableProducer(fileUri)

    def toCompilationUnitsSymbolTable: CompilationUnitsSymbolTable = stp.toCompilationUnitsSymbolTable

    def reportError(fileUri: String, line: Int, column: Int,
                    offset: Int, length: Int, message: String): Unit =
      stp.reportError(fileUri, line, column, offset, length, message)

    def reportWarning(fileUri: String, line: Int, column: Int,
                      offset: Int, length: Int, message: String): Unit =
      stp.reportWarning(fileUri, line, column, offset, length, message)
  }
  
  class CompilationUnitElementMiner(stp: CompilationUnitsSymbolTableProducer)
      extends StpWrapper(stp) {
    def mine(cu: CompilationUnit) = {
      val fileUri = cu.firstToken.fileUri
      val cut = tables.compilationUnitTable
      cut.get(fileUri) match {
        case Some(other) =>
          reportRedeclaration(fileUri, cu.firstToken, DUPLICATE_CU, other)
        case _ =>
          cut(fileUri) = cu
          tables.compilationUnitAbsTable(fileUri) = CompilationUnitSymbolTableData()
      }
      compilationUnit(fileUri, cu)
    }
    
    private def compilationUnit(fileUri: FileResourceUri, cu: CompilationUnit) {
      cu.topDecls.foreach{
        classOrInterfaceDeclaration(fileUri, _)
      }
    }
    
    private def classOrInterfaceDeclaration(fileUri: FileResourceUri, cd: ClassOrInterfaceDeclaration) {
      classOrInterfaceSymbol(fileUri, cd)
      val classOrInterfaceType = cd.typ
      cd.instanceFields.foreach{
        fieldSymbol(fileUri, classOrInterfaceType, _)
      }
      cd.staticFields.foreach {
        fieldSymbol(fileUri, classOrInterfaceType, _)
      }
      cd.methods.foreach {
        methodDeclaration(fileUri, classOrInterfaceType, _)
      }
    }
    
    private def classOrInterfaceSymbol(fileUri: FileResourceUri, classOrInterfaceDeclaration: ClassOrInterfaceDeclaration) = {
      val typ = classOrInterfaceDeclaration.typ
      val ct = tables.compilationUnitAbsTable(fileUri).classOrInterfaceTable
      ct.get(typ) match {
        case Some(other) =>
          reportRedeclaration(fileUri, classOrInterfaceDeclaration.nameID, DUPLICATE_CLASS, other)
        case _ =>
          ct(typ) = classOrInterfaceDeclaration
          tables.compilationUnitAbsTable(fileUri).classOrInterfaceAbsTable(typ) = ClassOrInterfaceSymbolTableData()
      }
    }
    
    private def fieldSymbol(fileUri: FileResourceUri, classOrInterfaceType: ObjectType, fieldDeclaration: Field with Declaration) = {
      val nameID = fieldDeclaration.nameID
      val ct = tables.compilationUnitAbsTable(fileUri).classOrInterfaceAbsTable(classOrInterfaceType)
      val ft = ct.fieldTable
      ft.get(nameID.text) match {
        case Some(other) =>
          reportRedeclaration(fileUri, nameID, DUPLICATE_FIELD, other)
        case _ =>
          ft(nameID.text.intern()) = fieldDeclaration
      }
    }
    
    private def methodDeclaration(fileUri: FileResourceUri, classOrInterfaceType: ObjectType, methodDeclaration: MethodDeclaration) = {
      methodSymbol(fileUri, classOrInterfaceType, methodDeclaration)
      val methodSig = methodDeclaration.signature
      methodDeclaration.paramClause.paramlist foreach {
        paramSymbol(fileUri, classOrInterfaceType, methodSig, _)
      }
      bodySymbol(fileUri, classOrInterfaceType, methodSig, methodDeclaration.body)
    }
     
    private def methodSymbol(fileUri: FileResourceUri, classOrInterfaceType: ObjectType, methodDeclaration: MethodDeclaration) = {
      val methodSig = methodDeclaration.signature
      val ct = tables.compilationUnitAbsTable(fileUri).classOrInterfaceAbsTable(classOrInterfaceType)
      val mt = ct.methodTable
      mt.get(methodSig) match {
        case Some(other) =>
          reportRedeclaration(methodDeclaration.nameID.fileUri, methodDeclaration.nameID, DUPLICATE_METHOD, other)
        case None =>
          mt(methodSig) = methodDeclaration
          ct.methodAbsTable(methodSig) = MethodSymbolTableData()
      }
    }
     
    private def paramSymbol(fileUri: FileResourceUri, classOrInterfaceType: ObjectType, methodSig: Signature, param: Param) = {
      val nameID = param.nameID
      val ct = tables.compilationUnitAbsTable(fileUri).classOrInterfaceAbsTable(classOrInterfaceType)
      val mt = ct.methodAbsTable(methodSig)
      val pl = mt.params
      pl.find(p => p.nameID.text == nameID.text) match {
        case Some(other) => 
          reportRedeclaration(nameID.fileUri, nameID, DUPLICATE_PARAM, other)
        case _ =>
          pl += param
      }
    }
    
    def bodySymbol(fileUri: FileResourceUri, classOrInterfaceType: ObjectType, methodSig: Signature, body: Body) = {
      body match {
        case rbody: ResolvedBody =>
          rbody.locals foreach {
            localVarSymbol(fileUri, classOrInterfaceType, methodSig, _)
          }
          val bodyTable = MethodBodySymbolTableData()
          tables.compilationUnitAbsTable(fileUri).classOrInterfaceAbsTable(classOrInterfaceType).methodAbsTable(methodSig).bodyTables = Some(bodyTable)
          rbody.locations foreach {
            locationSymbol(bodyTable, _)
          }
          rbody.catchClauses foreach {
            catchSymbol(bodyTable, _, rbody.locations)
          }
        case ubody: UnresolvedBody =>
          tables.compilationUnitAbsTable(fileUri).classOrInterfaceAbsTable(classOrInterfaceType).methodAbsTable(methodSig).bodyTables = None
      }
    }
     
    private def localVarSymbol(fileUri: FileResourceUri, classOrInterfaceType: ObjectType, methodSig: Signature, localVarDeclaration: LocalVarDeclaration) = {
      val nameID = localVarDeclaration.nameID
      val ct = tables.compilationUnitAbsTable(fileUri).classOrInterfaceAbsTable(classOrInterfaceType)
      val mt = ct.methodAbsTable(methodSig)
      val lt = mt.localVarTable
      lt.get(nameID.text) match {
        case Some(other) =>
          reportRedeclaration(nameID.fileUri, nameID, DUPLICATE_LOCAL_VAR, other)
        case _ =>
          lt(nameID.text) = localVarDeclaration
      }
    }
     
    private def locationSymbol(bodyTable: MethodBodySymbolTableData, location: JawaLocation) = {
      val locationUri = location.locationUri
      val lt = bodyTable.locationTable
      lt.get(locationUri) match {
        case Some(other) =>
          reportRedeclaration(location.locationID.fileUri, location.locationID, DUPLICATE_LOCATION, other)
        case _ =>
          lt(locationUri.intern()) = location
      }
    }
     
    private def catchSymbol(bodyTable: MethodBodySymbolTableData, catchClause: CatchClause, locations: ISeq[JawaLocation]) = {
      val start = bodyTable.locationTable(catchClause.range.fromLocation.text).locationIndex
      val end = bodyTable.locationTable(catchClause.range.toLocation.text).locationIndex
      if (end == -1) {
        reportError(catchClause.firstToken.fileUri, catchClause.range.toLocation.line,
          catchClause.range.toLocation.column, catchClause.range.toLocation.offset,
          catchClause.range.toLocation.length,
          CATCH_TABLE_END_BEFORE_START.
          format(catchClause.range.toLocation.text,
          catchClause.range.fromLocation.text,
          catchClause.range.fromLocation.line, catchClause.range.fromLocation.column))
       } else {
         val ct = bodyTable.catchTable
         for (i <- start to end)
           ct.getOrElseUpdate(locations(i).locationIndex, marrayEmpty) += catchClause
       }
    }
  }

  private def combineTable[T<: JawaAstNode](str: SymbolTableReporter,
                      m1: MMap[String, T],
                      m2: MMap[String, T],
                      f: T => Token,
                      message: String) =
    m2.foreach { p =>
      val key = p._1
      val value = p._2
      m1.get(key) match {
        case Some(ce) =>
          val ceName = f(ce)
          val otherName = f(value)
          str.reportError(ceName.fileUri, ceName, message.format(otherName.text, ceName.line,
              ceName.column, ceName.fileUri), value)
        case _ =>
          m1(key) = value
      }
    }

  def combine(stp1: CompilationUnitsSymbolTableProducer,
              stp2: CompilationUnitsSymbolTableProducer): CompilationUnitsSymbolTableProducer = {
    val tables1 = stp1.tables
    val tables2 = stp2.tables
    val str = stp1

    combineTable(str, tables1.compilationUnitTable, tables2.compilationUnitTable,
      { x: CompilationUnit => x.firstToken }, DUPLICATE_CU + OF_FILE)

    tables1.compilationUnitAbsTable ++= tables2.compilationUnitAbsTable
    stp1
  }
  
  def tearDown(tables: CompilationUnitsSymbolTableData, fileUri: FileResourceUri) = {
    tables.compilationUnitTable -= fileUri
    tables.compilationUnitAbsTable -= fileUri
  }
}