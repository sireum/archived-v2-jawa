package org.sireum.jawa.sjc.symtab

import org.sireum.util._
import org.sireum.jawa.sjc.parser._
import org.sireum.jawa.sjc.parser.{Location => JawaLocation}
import org.sireum.jawa.sjc.lexer.Token
import org.sireum.jawa.sjc.ObjectType
import org.sireum.jawa.sjc.Signature
import org.sireum.jawa.sjc.io.AbstractFile
import org.sireum.jawa.sjc.util.Position

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
    (file: AbstractFile): CompilationUnitSymbolTableProducer =
      stp.compilationUnitSymbolTableProducer(file)

    def toCompilationUnitsSymbolTable: CompilationUnitsSymbolTable = stp.toCompilationUnitsSymbolTable

    def reportError(pos: Position, message: String): Unit =
      error(pos, message)

    def reportWarning(pos: Position, message: String): Unit =
      warning(pos, message)

    def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
      stp.info(pos, msg, force)
    }
  }
  
  class CompilationUnitElementMiner(stp: CompilationUnitsSymbolTableProducer)
      extends StpWrapper(stp) {
    def mine(cu: CompilationUnit) = {
      val file = cu.firstToken.file.file
      stp.compilationUnitSymbolTableProducer(file)
      val cut = tables.compilationUnitTable
      cut.get(file) match {
        case Some(other) =>
          reportRedeclaration(file.toString(), cu.firstToken, DUPLICATE_CU, other)
        case _ =>
          cut(file) = cu          
          tables.compilationUnitAbsTable(file) = CompilationUnitSymbolTableData()
      }
      compilationUnit(file, cu)
    }
    
    private def compilationUnit(file: AbstractFile, cu: CompilationUnit) {
      cu.topDecls.foreach{
        classOrInterfaceDeclaration(file, _)
      }
    }
    
    private def classOrInterfaceDeclaration(file: AbstractFile, cd: ClassOrInterfaceDeclaration) {
      classOrInterfaceSymbol(file, cd)
      val classOrInterfaceType = cd.typ
      cd.instanceFields.foreach{
        fieldSymbol(file, classOrInterfaceType, _)
      }
      cd.staticFields.foreach {
        fieldSymbol(file, classOrInterfaceType, _)
      }
      cd.methods.foreach {
        methodDeclaration(file, classOrInterfaceType, _)
      }
    }
    
    private def classOrInterfaceSymbol(file: AbstractFile, classOrInterfaceDeclaration: ClassOrInterfaceDeclaration) = {
      val typ = classOrInterfaceDeclaration.typ
      stp.compilationUnitSymbolTableProducer(file).classOrInterfaceSymbolTableProducer(typ)
      val ct = tables.compilationUnitAbsTable(file).classOrInterfaceTable
      ct.get(typ) match {
        case Some(other) =>
          reportRedeclaration(file.toString(), classOrInterfaceDeclaration.cityp.firstToken, DUPLICATE_CLASS, other)
        case _ =>
          ct(typ) = classOrInterfaceDeclaration
          tables.compilationUnitAbsTable(file).classOrInterfaceAbsTable(typ) = ClassOrInterfaceSymbolTableData()
      }
    }
    
    private def fieldSymbol(file: AbstractFile, classOrInterfaceType: ObjectType, fieldDeclaration: Field with Declaration) = {
      val nameID = fieldDeclaration.nameID
      val ct = tables.compilationUnitAbsTable(file).classOrInterfaceAbsTable(classOrInterfaceType)
      val ft = ct.fieldTable
      ft.get(nameID.text) match {
        case Some(other) =>
          reportRedeclaration(file.toString(), nameID, DUPLICATE_FIELD, other)
        case _ =>
          ft(nameID.text.intern()) = fieldDeclaration
      }
    }
    
    private def methodDeclaration(file: AbstractFile, classOrInterfaceType: ObjectType, methodDeclaration: MethodDeclaration) = {
      methodSymbol(file, classOrInterfaceType, methodDeclaration)
      val methodSig = methodDeclaration.signature
      methodDeclaration.paramClause.paramlist foreach {
        paramSymbol(file, classOrInterfaceType, methodSig, _)
      }
      bodySymbol(file, classOrInterfaceType, methodSig, methodDeclaration.body)
    }
     
    private def methodSymbol(file: AbstractFile, classOrInterfaceType: ObjectType, methodDeclaration: MethodDeclaration) = {
      val methodSig = methodDeclaration.signature
      val ct = tables.compilationUnitAbsTable(file).classOrInterfaceAbsTable(classOrInterfaceType)
      stp.compilationUnitSymbolTableProducer(file).classOrInterfaceSymbolTableProducer(classOrInterfaceType).methodSymbolTableProducer(methodSig)
      val mt = ct.methodTable
      mt.get(methodSig) match {
        case Some(other) =>
          reportRedeclaration(methodDeclaration.nameID.file.toString(), methodDeclaration.nameID, DUPLICATE_METHOD, other)
        case None =>
          mt(methodSig) = methodDeclaration
          ct.methodAbsTable(methodSig) = MethodSymbolTableData()
      }
    }
     
    private def paramSymbol(file: AbstractFile, classOrInterfaceType: ObjectType, methodSig: Signature, param: Param) = {
      val nameID = param.nameID
      val ct = tables.compilationUnitAbsTable(file).classOrInterfaceAbsTable(classOrInterfaceType)
      val mt = ct.methodAbsTable(methodSig)
      val pl = mt.params
      pl.find(p => p.nameID.text == nameID.text) match {
        case Some(other) => 
          reportRedeclaration(nameID.file.toString(), nameID, DUPLICATE_PARAM, other)
        case _ =>
          pl += param
      }
    }
    
    def bodySymbol(file: AbstractFile, classOrInterfaceType: ObjectType, methodSig: Signature, body: Body) = {
      body match {
        case rbody: ResolvedBody =>
          rbody.locals foreach {
            localVarSymbol(file, classOrInterfaceType, methodSig, _)
          }
          val bodyTable = MethodBodySymbolTableData()
          tables.compilationUnitAbsTable(file).classOrInterfaceAbsTable(classOrInterfaceType).methodAbsTable(methodSig).bodyTables = Some(bodyTable)
          rbody.locations foreach {
            locationSymbol(bodyTable, _)
          }
          rbody.catchClauses foreach {
            catchSymbol(bodyTable, _, rbody.locations)
          }
        case ubody: UnresolvedBody =>
          tables.compilationUnitAbsTable(file).classOrInterfaceAbsTable(classOrInterfaceType).methodAbsTable(methodSig).bodyTables = None
      }
    }
     
    private def localVarSymbol(file: AbstractFile, classOrInterfaceType: ObjectType, methodSig: Signature, localVarDeclaration: LocalVarDeclaration) = {
      val nameID = localVarDeclaration.nameID
      val ct = tables.compilationUnitAbsTable(file).classOrInterfaceAbsTable(classOrInterfaceType)
      val mt = ct.methodAbsTable(methodSig)
      val lt = mt.localVarTable
      lt.get(nameID.text) match {
        case Some(other) =>
          reportRedeclaration(nameID.file.toString(), nameID, DUPLICATE_LOCAL_VAR, other)
        case _ =>
          lt(nameID.text) = localVarDeclaration
      }
    }
     
    private def locationSymbol(bodyTable: MethodBodySymbolTableData, location: JawaLocation) = {
      val locationUri = location.locationUri
      val lt = bodyTable.locationTable
      lt.get(locationUri) match {
        case Some(other) =>
          reportRedeclaration(location.locationID.file.toString(), location.locationID, DUPLICATE_LOCATION, other)
        case _ =>
          lt(locationUri.intern()) = location
      }
    }
     
    private def catchSymbol(bodyTable: MethodBodySymbolTableData, catchClause: CatchClause, locations: ISeq[JawaLocation]) = {
      val start = bodyTable.locationTable(catchClause.range.fromLocation.text).locationIndex
      val end = bodyTable.locationTable(catchClause.range.toLocation.text).locationIndex
      if (end == -1) {
        reportError(catchClause.firstToken.pos,
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
                      m1: MMap[AbstractFile, T],
                      m2: MMap[AbstractFile, T],
                      f: T => Token,
                      message: String) =
    m2.foreach { p =>
      val key = p._1
      val value = p._2
      m1.get(key) match {
        case Some(ce) =>
          val ceName = f(ce)
          val otherName = f(value)
          str.reportError(ceName.file.toString(), ceName, message.format(otherName.text, ceName.line,
              ceName.column, ceName.file), value)
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
    tables1.asInstanceOf[JawaCompilationUnitsSymbolTable].cuMap ++= tables2.asInstanceOf[JawaCompilationUnitsSymbolTable].cuMap
    stp1
  }
  
  def tearDown(tables: CompilationUnitsSymbolTableData, file: AbstractFile) = {
    tables.compilationUnitTable -= file
    tables.compilationUnitAbsTable -= file
  }
}