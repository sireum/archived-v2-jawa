package org.sireum.jawa.symtab

import org.sireum.util._
import org.sireum.jawa.parser._
import org.sireum.jawa.parser.{Location => JawaLocation}

class JawaCompilationUnitSymbolTable extends CompilationUnitSymbolTable with CompilationUnitSymbolTableProducer {
  st =>
  val tables = CompilationUnitSymbolTableData()
  val tags = marrayEmpty[LocationTag]
  var hasErrors = false

  val ERROR_TAG_TYPE = MarkerType(
    "org.sireum.pilar.tag.error.symtab",
    None,
    "Pilar Symbol Resolution Error",
    MarkerTagSeverity.Error,
    MarkerTagPriority.Normal,
    ilist(MarkerTagKind.Problem, MarkerTagKind.Text))

  val WARNING_TAG_TYPE = MarkerType(
    "org.sireum.pilar.tag.error.symtab",
    None,
    "Pilar Symbol Resolution Warning",
    MarkerTagSeverity.Warning,
    MarkerTagPriority.Normal,
    ilist(MarkerTagKind.Problem, MarkerTagKind.Text))

  def reportError(source : Option[FileResourceUri], line : Int,
                    column : Int, message : String) : Unit = {
      tags += Tag.toTag(source, line, column, message, ERROR_TAG_TYPE)
      hasErrors = true
    }

  def reportWarning(fileUri : Option[String], line : Int,
                    column : Int, message : String) : Unit =
    tags += Tag.toTag(fileUri, line, column, message, WARNING_TAG_TYPE)

  def reportError(source : Option[FileResourceUri], line : Int,
                  column : Int, offset : Int, length : Int,
                  message : String) : Unit = {
    tags += Tag.toTag(source, line, column, offset, length, message,
      ERROR_TAG_TYPE)
    hasErrors = true
  }

  def reportWarning(fileUri : Option[String], line : Int,
                    column : Int, offset : Int, length : Int,
                    message : String) : Unit =
    tags += Tag.toTag(fileUri, line, column, offset, length, message,
      WARNING_TAG_TYPE)

  val ciMap = mmapEmpty[ResourceUri, JawaClassOrInterfaceSymbolTable]
  def classOrInterfaceNames: Iterable[String] = tables.classOrInterfaceTable.keys
  def classOrInterfaces: Iterable[ClassOrInterfaceDeclaration] = tables.classOrInterfaceTable.values
  def classOrInterface(classOrInterfaceName: String): ClassOrInterfaceDeclaration = tables.classOrInterfaceTable(classOrInterfaceName)
  def classOrInterfaceSymbolTables : Iterable[ClassOrInterfaceSymbolTable] = ciMap.values
  def classOrInterfaceSymbolTable(classOrInterfaceName: String): ClassOrInterfaceSymbolTable = classOrInterfaceSymbolTableProducer(classOrInterfaceName)
  def classOrInterfaceSymbolTableProducer(classOrInterfaceName: String) = {
    assert(tables.classOrInterfaceAbsTable.contains(classOrInterfaceName))
    ciMap.getOrElseUpdate(classOrInterfaceName, new JawaClassOrInterfaceSymbolTable(classOrInterfaceName, st))
  }
  def toCompilationUnitSymbolTable: CompilationUnitSymbolTable = this
}

class JawaClassOrInterfaceSymbolTable(val classOrInterfaceName : String, cust : CompilationUnitSymbolTable) extends ClassOrInterfaceSymbolTable with ClassOrInterfaceSymbolTableProducer {
  st =>
  val tables = ClassOrInterfaceSymbolTableData()
  def cuSymbolTable: CompilationUnitSymbolTable = cust
  def cuSymbolTableProducer: CompilationUnitSymbolTableProducer = cust.asInstanceOf[CompilationUnitSymbolTableProducer]

  def classOrInterfaceDecl: ClassOrInterfaceDeclaration = cuSymbolTable.classOrInterface(classOrInterfaceName)
  def fieldNames: Iterable[String] = tables.fieldTable.keys
  def fieldDecls: Iterable[Field with Declaration] = tables.fieldTable.values
  def fieldDecl(fieldSig: String): Field with Declaration = tables.fieldTable(fieldSig)
  def staticFieldNames: Iterable[String] = tables.fieldTable.filter{case (name, fd) => fd.isStatic}.keys
  def staticFieldDecls: Iterable[StaticFieldDeclaration] = tables.fieldTable.filter{case (name, fd) => fd.isStatic}.values.map(_.asInstanceOf[StaticFieldDeclaration])
  def instanceFieldNames: Iterable[String] = tables.fieldTable.filter{case (name, fd) => !fd.isStatic}.keys
  def instanceFieldDecls: Iterable[InstanceFieldDeclaration] = tables.fieldTable.filter{case (name, fd) => !fd.isStatic}.values.map(_.asInstanceOf[InstanceFieldDeclaration])
  def methodNames: Iterable[String] = tables.methodTable.values.map(_.nameID.text)
  def methodSigs: Iterable[String] = tables.methodTable.keys
  def methodDecls: Iterable[MethodDeclaration] = tables.methodTable.values
  def methodDecl(methodSig: String): MethodDeclaration = tables.methodTable(methodSig)
  val mtMap = mmapEmpty[ResourceUri, JawaMethodSymbolTable]
  def methodSymbolTables: Iterable[MethodSymbolTable] = mtMap.values
  def methodSymbolTable(methodSig: String): MethodSymbolTable = methodSymbolTableProducer(methodSig)
  def methodSymbolTableProducer(methodSig: String) = {
    assert(tables.methodAbsTable.contains(methodSig))
    mtMap.getOrElseUpdate(classOrInterfaceName, new JawaMethodSymbolTable(methodSig, st))
  }
}

class JawaMethodSymbolTable(methodSig: String, clst: JawaClassOrInterfaceSymbolTable) extends MethodSymbolTable with MethodSymbolTableProducer {
  def tables: MethodSymbolTableData = MethodSymbolTableData()

  def ciSymbolTable: ClassOrInterfaceSymbolTable = clst
  def ciSymbolTableProducer: ClassOrInterfaceSymbolTableProducer = clst.asInstanceOf[ClassOrInterfaceSymbolTableProducer]

  def methodName: String = ciSymbolTable.methodDecl(methodSig).nameID.text
  def methodDecl: MethodDeclaration = ciSymbolTable.methodDecl(methodSig)
  def paramNames: ISeq[String] = tables.params.map(_.nameID.text).toList
  def params: ISeq[Param] = tables.params.toList
  def isParam(varname: String): Boolean = paramNames.contains(varname)
  def param(paramName: String): Param = tables.params.find { x => ??? }
  def localNames: Iterable[String] = tables.localVarTable.keys
  def locals: Iterable[LocalVarDeclaration] = tables.localVarTable.values
  def local(localName: String): LocalVarDeclaration = tables.localVarTable(localName)
  def locations: ISeq[JawaLocation] = 
    tables.bodyTables match {
      case Some(bt) => methodDecl.body.locations
      case _        => ivectorEmpty
    }
  def location(locationUri : String): JawaLocation =
    tables.bodyTables.get.locationTable(locationUri)
  def catchClauses(locationIndex : Int) : Iterable[CatchClause] =
    tables.bodyTables.get.catchClauses.getOrElse(locationIndex,
      Array.empty[CatchClause] : Iterable[CatchClause])
}