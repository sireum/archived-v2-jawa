package org.sireum.jawa.sjc.symtab

import org.sireum.util._
import org.sireum.jawa.sjc.parser._
import org.sireum.jawa.sjc.parser.{Location => JawaLocation}
import org.sireum.jawa.sjc.ObjectType
import org.sireum.jawa.sjc.Signature

class JawaCompilationUnitsSymbolTable extends CompilationUnitsSymbolTable with CompilationUnitsSymbolTableProducer {
  st =>
    
  val tables = CompilationUnitsSymbolTableData()
  val tags = marrayEmpty[LocationTag]
  var hasErrors = false

  val ERROR_TAG_TYPE = MarkerType(
    "org.sireum.jawa.sjc.tag.error.symtab",
    None,
    "Jawa Symbol Resolution Error",
    MarkerTagSeverity.Error,
    MarkerTagPriority.Normal,
    ilist(MarkerTagKind.Problem, MarkerTagKind.Text))

  val WARNING_TAG_TYPE = MarkerType(
    "org.sireum.jawa.sjc.tag.error.symtab",
    None,
    "Jawa Symbol Resolution Warning",
    MarkerTagSeverity.Warning,
    MarkerTagPriority.Normal,
    ilist(MarkerTagKind.Problem, MarkerTagKind.Text))

  def reportError(source: FileResourceUri, line: Int,
                    column: Int, message: String): Unit = {
      tags += Tag.toTag(Some(source), line, column, message, ERROR_TAG_TYPE)
      hasErrors = true
    }

  def reportWarning(fileUri: String, line: Int,
                    column: Int, message: String): Unit =
    tags += Tag.toTag(Some(fileUri), line, column, message, WARNING_TAG_TYPE)

  def reportError(source: FileResourceUri, line: Int,
                  column: Int, offset: Int, length: Int,
                  message: String): Unit = {
    tags += Tag.toTag(Some(source), line, column, offset, length, message,
      ERROR_TAG_TYPE)
    hasErrors = true
  }

  def reportWarning(fileUri: String, line: Int,
                    column: Int, offset: Int, length: Int,
                    message: String): Unit =
    tags += Tag.toTag(Some(fileUri), line, column, offset, length, message,
      WARNING_TAG_TYPE)
      
  

  val cuMap = mmapEmpty[FileResourceUri, JawaCompilationUnitSymbolTable]
  def fileUris: Iterable[FileResourceUri] = tables.compilationUnitTable.keys
  def compilationUnits: Iterable[CompilationUnit] = tables.compilationUnitTable.values
  def compilationUnit(fileUri: FileResourceUri): CompilationUnit = tables.compilationUnitTable(fileUri)
  def compilationUnitSymbolTables: Iterable[CompilationUnitSymbolTable] = cuMap.values
  def compilationUnitSymbolTable(fileUri: FileResourceUri): CompilationUnitSymbolTable = cuMap(fileUri)
  def compilationUnitSymbolTableProducer(fileUri: FileResourceUri) = {
    assert(tables.compilationUnitAbsTable.contains(fileUri))
    cuMap.getOrElseUpdate(fileUri, new JawaCompilationUnitSymbolTable(fileUri, st))
  }
  def toCompilationUnitsSymbolTable: CompilationUnitsSymbolTable = this
}

class JawaCompilationUnitSymbolTable(val fileUri: FileResourceUri, cusst: CompilationUnitsSymbolTable) extends CompilationUnitSymbolTable with CompilationUnitSymbolTableProducer {
  st =>
  val tables = CompilationUnitSymbolTableData()
  def cusSymbolTable: CompilationUnitsSymbolTable = cusst
  def cusSymbolTableProducer: CompilationUnitsSymbolTableProducer = cusst.asInstanceOf[CompilationUnitsSymbolTableProducer]

  val ciMap = mmapEmpty[ObjectType, JawaClassOrInterfaceSymbolTable]
  def classOrInterfaceTypes: Iterable[ObjectType] = tables.classOrInterfaceTable.keys
  def classOrInterfaces: Iterable[ClassOrInterfaceDeclaration] = tables.classOrInterfaceTable.values
  def classOrInterface(classOrInterfaceType: ObjectType): ClassOrInterfaceDeclaration = tables.classOrInterfaceTable(classOrInterfaceType)
  def classOrInterfaceSymbolTables: Iterable[ClassOrInterfaceSymbolTable] = ciMap.values
  def classOrInterfaceSymbolTable(classOrInterfaceType: ObjectType): ClassOrInterfaceSymbolTable = classOrInterfaceSymbolTableProducer(classOrInterfaceType)
  def classOrInterfaceSymbolTableProducer(classOrInterfaceType: ObjectType) = {
    assert(tables.classOrInterfaceAbsTable.contains(classOrInterfaceType))
    ciMap.getOrElseUpdate(classOrInterfaceType, new JawaClassOrInterfaceSymbolTable(classOrInterfaceType, st))
  }
  def toCompilationUnitSymbolTable: CompilationUnitSymbolTable = this
}

class JawaClassOrInterfaceSymbolTable(val classOrInterfaceType: ObjectType, cust: CompilationUnitSymbolTable) extends ClassOrInterfaceSymbolTable with ClassOrInterfaceSymbolTableProducer {
  st =>
  val tables = ClassOrInterfaceSymbolTableData()
  def cuSymbolTable: CompilationUnitSymbolTable = cust
  def cuSymbolTableProducer: CompilationUnitSymbolTableProducer = cust.asInstanceOf[CompilationUnitSymbolTableProducer]

  def classOrInterfaceDecl: ClassOrInterfaceDeclaration = cuSymbolTable.classOrInterface(classOrInterfaceType)
  def fieldNames: Iterable[String] = tables.fieldTable.keys
  def fieldDecls: Iterable[Field with Declaration] = tables.fieldTable.values
  def fieldDecl(fieldSig: String): Field with Declaration = tables.fieldTable(fieldSig)
  def staticFieldNames: Iterable[String] = tables.fieldTable.filter{case (name, fd) => fd.isStatic}.keys
  def staticFieldDecls: Iterable[StaticFieldDeclaration] = tables.fieldTable.filter{case (name, fd) => fd.isStatic}.values.map(_.asInstanceOf[StaticFieldDeclaration])
  def instanceFieldNames: Iterable[String] = tables.fieldTable.filter{case (name, fd) => !fd.isStatic}.keys
  def instanceFieldDecls: Iterable[InstanceFieldDeclaration] = tables.fieldTable.filter{case (name, fd) => !fd.isStatic}.values.map(_.asInstanceOf[InstanceFieldDeclaration])
  def methodNames: Iterable[String] = tables.methodTable.values.map(_.nameID.text)
  def methodSigs: Iterable[Signature] = tables.methodTable.keys
  def methodDecls: Iterable[MethodDeclaration] = tables.methodTable.values
  def methodDecl(methodSig: Signature): MethodDeclaration = tables.methodTable(methodSig)
  val mtMap = mmapEmpty[Signature, JawaMethodSymbolTable]
  def methodSymbolTables: Iterable[MethodSymbolTable] = mtMap.values
  def methodSymbolTable(methodSig: Signature): MethodSymbolTable = methodSymbolTableProducer(methodSig)
  def methodSymbolTableProducer(methodSig: Signature) = {
    assert(tables.methodAbsTable.contains(methodSig))
    mtMap.getOrElseUpdate(methodSig, new JawaMethodSymbolTable(methodSig, st))
  }
}

class JawaMethodSymbolTable(val methodSig: Signature, clst: JawaClassOrInterfaceSymbolTable) extends MethodSymbolTable with MethodSymbolTableProducer {
  def tables: MethodSymbolTableData = MethodSymbolTableData()

  def ciSymbolTable: ClassOrInterfaceSymbolTable = clst
  def ciSymbolTableProducer: ClassOrInterfaceSymbolTableProducer = clst.asInstanceOf[ClassOrInterfaceSymbolTableProducer]

  def methodName: String = ciSymbolTable.methodDecl(methodSig).nameID.text
  def methodDecl: MethodDeclaration = ciSymbolTable.methodDecl(methodSig)
  def thisNameOpt: Option[String] = thisOpt.map(_.name)
  def thisOpt: Option[Param] = {
    tables.params.find(_.isThis)
  }
  def paramNames: ISeq[String] = params.map(_.name).toList
  def params: ISeq[Param] = tables.params.filter(!_.isThis).toList
  def isParam(varname: String): Boolean = paramNames.contains(varname)
  def param(paramName: String): Param = tables.params.find { x => x.nameID.text == paramName }.get
  def localNames: Iterable[String] = tables.localVarTable.keys
  def locals: Iterable[LocalVarDeclaration] = tables.localVarTable.values
  def local(localName: String): LocalVarDeclaration = tables.localVarTable(localName)
  def locations: ISeq[JawaLocation] = 
    tables.bodyTables match {
      case Some(bt) => methodDecl.body.asInstanceOf[ResolvedBody].locations
      case _        => ivectorEmpty
    }
  def location(locationUri: String): JawaLocation =
    tables.bodyTables.get.locationTable(locationUri)
  def location(locationIndex: Int): JawaLocation = locations(locationIndex)
  def catchClauses(locationIndex: Int): Iterable[CatchClause] =
    tables.bodyTables.get.catchTable.getOrElse(locationIndex,
      Array.empty[CatchClause]: Iterable[CatchClause])
}