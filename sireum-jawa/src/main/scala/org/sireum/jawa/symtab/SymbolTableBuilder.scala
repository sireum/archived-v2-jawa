package org.sireum.jawa.symtab

import org.sireum.util._
import org.sireum.jawa.parser._
import org.sireum.jawa.parser.{Location => JawaLocation}
import org.sireum.jawa.lexer.Token

object SymbolTableBuilder {
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
  
  abstract class StpWrapper(stp: CompilationUnitSymbolTableProducer) extends CompilationUnitSymbolTableProducer {

    def tables: CompilationUnitSymbolTableData = stp.tables

    def classOrInterfaceSymbolTableProducer
    (classOrInterfaceName: String): ClassOrInterfaceSymbolTableProducer =
      stp.classOrInterfaceSymbolTableProducer(classOrInterfaceName)

    def toCompilationUnitSymbolTable: CompilationUnitSymbolTable = stp.toCompilationUnitSymbolTable

    def reportError(fileUri: Option[String], line: Int, column: Int,
                    offset: Int, length: Int, message: String): Unit =
      stp.reportError(fileUri, line, column, offset, length, message)

    def reportWarning(fileUri: Option[String], line: Int, column: Int,
                      offset: Int, length: Int, message: String): Unit =
      stp.reportWarning(fileUri, line, column, offset, length, message)
  }
  
  class CompilationUnitElementMiner(stp: CompilationUnitSymbolTableProducer)
      extends StpWrapper(stp) {
    def mine(cu: CompilationUnit) = {
      cu.topDecls.foreach{
        classOrInterfaceDeclaration(_)
      }
    }
    
    def classOrInterfaceDeclaration(cd: ClassOrInterfaceDeclaration) {
      classOrInterfaceSymbol(cd)
      val classOrInterfaceNameID = cd.nameID
      cd.instanceFields.foreach{
        fieldSymbol(classOrInterfaceNameID, _)
      }
      cd.staticFields.foreach {
        fieldSymbol(classOrInterfaceNameID, _)
      }
      cd.methods.foreach {
        methodDeclaration(classOrInterfaceNameID, _)
      }
    }
    
    private def classOrInterfaceSymbol(classOrInterfaceDeclaration: ClassOrInterfaceDeclaration) = {
      val nameID = classOrInterfaceDeclaration.nameID
      val ct = tables.classOrInterfaceTable
      ct.get(nameID.text) match {
        case Some(other) =>
          reportRedeclaration(nameID.fileUriOpt, nameID, DUPLICATE_CLASS, other)
        case _ =>
          ct(nameID.text.intern()) = classOrInterfaceDeclaration
      }
    }
    
    private def fieldSymbol(classOrInterfaceNameID: Token, fieldDeclaration: Field with Declaration) = {
      val nameID = fieldDeclaration.nameID
      val ct = tables.classOrInterfaceAbsTable(classOrInterfaceNameID.text)
      val ft = ct.fieldTable
      ft.get(nameID.text) match {
        case Some(other) =>
          reportRedeclaration(nameID.fileUriOpt, nameID, DUPLICATE_FIELD, other)
        case _ =>
          ft(nameID.text.intern()) = fieldDeclaration
      }
    }
    
     private def methodDeclaration(classOrInterfaceNameID: Token, methodDeclaration: MethodDeclaration) = {
       methodSymbol(classOrInterfaceNameID, methodDeclaration)
       val methodSig = methodDeclaration.signature
       methodDeclaration.paramClause.paramlist foreach {
         paramSymbol(classOrInterfaceNameID, methodSig, _)
       }
       methodDeclaration.body.locals foreach {
         localVarSymbol(classOrInterfaceNameID, methodSig, _)
       }
       if(methodDeclaration.body.isEmptyBody){
         tables.classOrInterfaceAbsTable(classOrInterfaceNameID.text).methodAbsTable(methodSig).bodyTables = None
       } else {
         val bodyTable = new MethodBodySymbolTableData
         tables.classOrInterfaceAbsTable(classOrInterfaceNameID.text).methodAbsTable(methodSig).bodyTables = Some(bodyTable)
         methodDeclaration.body.locations foreach {
           locationSymbol(bodyTable, _)
         }
         methodDeclaration.body.catchClauses foreach {
           catchSymbol(bodyTable, _)
         }
       }
     }
     
     private def methodSymbol(classOrInterfaceNameID: Token, methodDeclaration: MethodDeclaration) = {
       val methodSig = methodDeclaration.signature
       val ct = tables.classOrInterfaceAbsTable(classOrInterfaceNameID.text)
       val mt = ct.methodTable
       mt.get(methodSig) match {
         case Some(other) =>
           reportRedeclaration(methodDeclaration.nameID.fileUriOpt, methodDeclaration.nameID, DUPLICATE_METHOD, other)
         case None =>
           mt(methodSig) = methodDeclaration
       }
     }
     
     private def paramSymbol(classOrInterfaceNameID: Token, methodSig: String, param: Param) = {
       val nameID = param.nameID
       val ct = tables.classOrInterfaceAbsTable(classOrInterfaceNameID.text)
       val mt = ct.methodAbsTable(methodSig)
       val pl = mt.params
       pl.find(p => p.nameID.text == nameID.text) match {
         case Some(other) => 
           reportRedeclaration(nameID.fileUriOpt, nameID, DUPLICATE_PARAM, other)
         case _ =>
           pl += param
       }
     }
     
     private def localVarSymbol(classOrInterfaceNameID: Token, methodSig: String, localVarDeclaration: LocalVarDeclaration) = {
       val nameID = localVarDeclaration.nameID
       val ct = tables.classOrInterfaceAbsTable(classOrInterfaceNameID.text)
       val mt = ct.methodAbsTable(methodSig)
       val lt = mt.localVarTable
       lt.get(nameID.text) match {
         case Some(other) =>
           reportRedeclaration(nameID.fileUriOpt, nameID, DUPLICATE_LOCAL_VAR, other)
         case _ =>
           lt(nameID.text) = localVarDeclaration
       }
     }
     
     private def locationSymbol(bodyTable: MethodBodySymbolTableData, location: JawaLocation) = {
       val locationID = location.locationID
       val lt = bodyTable.locationTable
       lt.get(locationID.text) match {
         case Some(other) =>
           reportRedeclaration(locationID.fileUriOpt, locationID, DUPLICATE_LOCATION, other)
         case _ =>
           lt(locationID.text) = location
       }
     }
     
     private def catchSymbol(bodyTable: MethodBodySymbolTableData, catchClause: CatchClause) = {
       val ccs = bodyTable.catchClauses
       ccs += catchClause
     }
  }

  def combineTable[T<: JawaAstNode](str : SymbolTableReporter,
                      m1 : MMap[String, T],
                      m2 : MMap[String, T],
                      f : T => Token,
                      message : String) =
    m2.foreach { p =>
      val key = p._1
      val value = p._2
      m1.get(key) match {
        case Some(ce) =>
          val ceName = f(ce)
          val otherName = f(value)
          str.reportError(ceName.fileUriOpt, ceName, message.format(otherName.text, ceName.line,
              ceName.column, ceName.fileUriOpt), value)
        case _ =>
          m1(key) = value
      }
    }

  def combine(stp1 : CompilationUnitSymbolTableProducer,
              stp2 : CompilationUnitSymbolTableProducer) : CompilationUnitSymbolTableProducer = {
    val tables1 = stp1.tables
    val tables2 = stp2.tables
    val str = stp1

    combineTable(str, tables1.classOrInterfaceTable, tables2.classOrInterfaceTable,
      { x : ClassOrInterfaceDeclaration => x.nameID }, DUPLICATE_CLASS + OF_FILE)

    tables1.classOrInterfaceAbsTable ++= tables2.classOrInterfaceAbsTable
    
    stp1
  }
  
  def tearDown(tables : CompilationUnitSymbolTableData, cu : CompilationUnit) = {
    val fileUri = cu.firstToken.fileUriOpt.get
    val declaredSymbols = tables.declaredSymbols(fileUri)
    tables.declaredSymbols -= fileUri
    tables.classOrInterfaceTable --= declaredSymbols
    tables.classOrInterfaceAbsTable --= declaredSymbols
  }
}