/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.interactive

import org.sireum.util._
import scala.collection.GenMap
import scala.collection.Parallel
import org.sireum.jawa.sjc.ResolveLevel
import org.sireum.jawa.sjc.Signature
import org.sireum.jawa.sjc.JavaKnowledge
import org.sireum.jawa.sjc.ObjectType
import org.sireum.jawa.sjc.PrimitiveType
import org.sireum.jawa.sjc.AccessFlag
import org.sireum.jawa.sjc.symtab.ClassOrInterfaceSymbolTable
import org.sireum.jawa.sjc.parser.ClassOrInterfaceDeclaration
import scala.collection.GenIterable
import org.sireum.jawa.sjc.parser._
import org.sireum.jawa.sjc.JawaType
import org.sireum.jawa.sjc.symtab.MethodSymbolTable
import org.sireum.jawa.sjc.lexer.Token
import org.sireum.jawa.sjc.symtab.MethodBodySymbolTableData
import org.sireum.jawa.sjc.symtab.JawaCompilationUnitsSymbolTable
import org.sireum.jawa.sjc.symtab.CompilationUnitSymbolTable
import org.sireum.jawa.sjc.symtab.JawaCompilationUnitsSymbolTableBuilder
import org.sireum.jawa.sjc.symtab.CompilationUnitsSymbolTable
import org.sireum.jawa.sjc.symtab.MethodSymbolTableProducer
import org.sireum.jawa.sjc.symtab.SymbolTableHelper
import org.sireum.jawa.sjc.io.AbstractFile
import org.sireum.jawa.sjc.util.NoPosition
import org.sireum.jawa.sjc.util.SourceFile

/**
 * this object collects info from the symbol table and builds Global, JawaClass, and JawaMethod
 *
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait JawaResolver extends JawaClasspathManager with JavaKnowledge with ResolveLevel {self: Global =>
  
  val DEBUG: Boolean = false
  private final val TITLE: String = "JawaResolver"
  
  import scala.reflect.runtime.{ universe => ru }
  var fst = { _: Unit => new JawaCompilationUnitsSymbolTable }
  
  private def parseCode[T <: ParsableAstNode : ru.TypeTag](source: AbstractFile, resolveBody: Boolean): Option[T] = {
    val paopt = JawaParser.parse[T](Right(source), resolveBody, reporter) 
    paopt
  }
  
  private def parseBodyTokens(bodyTokens: IList[Token]): Option[ResolvedBody] = {
    val paopt = JawaParser.parse[Body](bodyTokens, true, reporter) 
    paopt map{pa => pa.asInstanceOf[ResolvedBody]}
  }
    
  def getCompilationUnitsSymbolResult(sources: ISeq[AbstractFile], desiredLevel: ResolveLevel.Value): CompilationUnitsSymbolTable = {
    val changedOrDelatedFiles: ISet[AbstractFile] = sources.toSet
    val resolveBody: Boolean = desiredLevel match{case ResolveLevel.BODY => true; case _ => false}
    val newCUs: MList[CompilationUnit] = mlistEmpty
    sources.foreach(s => parseCode[CompilationUnit](s, resolveBody).foreach(newCUs += _))
    val delta: JawaDelta = JawaDelta(changedOrDelatedFiles, newCUs.toList)
    JawaCompilationUnitsSymbolTableBuilder(this, delta, fst, true)
  }
  
  def getCompilationUnitSymbolResult(source: AbstractFile, desiredLevel: ResolveLevel.Value): CompilationUnitSymbolTable = {
    val cst = getCompilationUnitsSymbolResult(List(source), desiredLevel)
    cst.compilationUnitSymbolTable(source)
  }
  
//  def getCompilationUnitSymbolResult(source: AbstractFile, desiredLevel: ResolveLevel.Value): Option[CompilationUnitSymbolTable] = {
//    val resolveBody = desiredLevel match {
//      case ResolveLevel.BODY => true
//      case _ => false
//    }
//    JawaParser.parse[CompilationUnit](Right(source), resolveBody, reporter) map{
//      cu => 
//        JawaCompilationUnitsSymbolTableBuilder(List(cu), fst, true).compilationUnitSymbolTable(source)
//    }
//  }
  
  /**
   * resolve the given method's body to BODY level. 
   */
  def resolveMethodBody(mst: MethodSymbolTable): MethodSymbolTable = {
    parseBodyTokens(mst.methodDecl.body.tokens) map {
      body => JawaCompilationUnitsSymbolTableBuilder(mst, body)
    }
    if(reporter.hasErrors) reporter.error(NoPosition, "Fail to resolve method body for " + mst.methodSig)
    mst
  }
    
  /**
   * resolve the given classes to desired level. 
   */
  def tryResolveClass(typ: ObjectType, desiredLevel: ResolveLevel.Value): Option[JawaClass] = {
    if(containsClassFile(typ)){
	    val r = desiredLevel match{
	      case ResolveLevel.BODY => resolveToBody(typ)
	      case ResolveLevel.HIERARCHY => resolveToHierarchy(typ)
	    }
	    Some(r)
    } else {
      None
    }
  }
    
  /**
   * resolve the given classes to desired level. 
   */
  def resolveClass(typ: ObjectType, desiredLevel: ResolveLevel.Value): JawaClass = {
    if(!typ.isArray && !containsClassFile(typ)){
      if(!containsClass(typ) || getClass(typ).get.getResolvingLevel < desiredLevel){
	      val rec = JawaClass(this, typ, 0)
	      rec.setUnknown
	      rec.setResolvingLevel(desiredLevel)
	      rec
      } else getClass(typ).get
    } else {
	    desiredLevel match{
	      case ResolveLevel.BODY => resolveToBody(typ)
	      case ResolveLevel.HIERARCHY => resolveToHierarchy(typ)
	    }
    }
  }
  
  /**
   * resolve the given classes to desired level. 
   */
  def resolveClassFromSource(source: SourceFile, desiredLevel: ResolveLevel.Value): ISet[JawaClass] = {
    val st = getCompilationUnitSymbolResult(source.file, desiredLevel)
    val tpes = source.getClassTypes(reporter)
    tpes foreach{removeClass(_)}
    resolveFromST(st, desiredLevel, true)
    tpes map{tpe => getClass(tpe).get}
  }
  
  /**
   * resolve the given classes to desired level. 
   */
  def forceResolveClass(typ: ObjectType, desiredLevel: ResolveLevel.Value): JawaClass = {
    desiredLevel match{
      case ResolveLevel.BODY => forceResolveToBody(typ)
      case ResolveLevel.HIERARCHY => forceResolveToHierarchy(typ)
    }
  }
  
  /**
   * resolve the given class to hierarchy level
   */
  def resolveToHierarchy(typ: ObjectType): JawaClass = {
    if(!containsClass(typ) || getClass(typ).get.getResolvingLevel < ResolveLevel.HIERARCHY) forceResolveToHierarchy(typ)
    getClass(typ).get
  }
  
  /**
   * force resolve the given class to hierarchy level
   */
  private def forceResolveToHierarchy(typ: ObjectType): JawaClass = {
    if(typ.isArray){
      resolveArrayClass(typ)
    } else {
	    val file = getClassFile(typ)
	    val st = getCompilationUnitSymbolResult(file, ResolveLevel.HIERARCHY)
	    removeClass(typ)
	    resolveFromST(st, ResolveLevel.HIERARCHY, true)
    }
    getClass(typ).get
  }
  
  /**
   * resolve the given class to body level
   */
  def resolveToBody(typ: ObjectType): JawaClass = {
    if(!containsClass(typ)) forceResolveToBody(typ)
    else if(getClass(typ).get.getResolvingLevel < ResolveLevel.BODY) escalateResolvingLevel(getClass(typ).get, ResolveLevel.BODY)
    else getClass(typ).get
  }
  
  /**
   * escalate resolving level
   */
  private def escalateResolvingLevel(rec: JawaClass, desiredLevel: ResolveLevel.Value): JawaClass = {
    require(rec.getResolvingLevel < desiredLevel)
    if(desiredLevel == ResolveLevel.BODY){
//      rec.getMethods.foreach(_.tryResolveBody)
      rec.setResolvingLevel(ResolveLevel.BODY)
    }
    rec
  }
  
  /**
   * force resolve the given class to body level
   */
  private def forceResolveToBody(typ: ObjectType): JawaClass = {
    if(typ.isArray){
      resolveArrayClass(typ)
    } else {
	    val file = getClassFile(typ)
	    val st = getCompilationUnitSymbolResult(file, ResolveLevel.BODY)
	    removeClass(typ)
	    resolveFromST(st, ResolveLevel.BODY, true)
    }
    getClass(typ).get
  }
  
  /**
   * resolve array class
   */
  private def resolveArrayClass(typ: ObjectType): Unit = {
    val classAccessFlag =	
      if(isJavaPrimitive(typ.typ)){
      	"FINAL_PUBLIC"
	    } else {
	      val base = resolveClass(new ObjectType(typ.typ), ResolveLevel.HIERARCHY)
	      val baseaf = base.getAccessFlagsStr
	      if(baseaf.contains("FINAL")) baseaf else "FINAL_" + baseaf
	    }
    val clazz: JawaClass = JawaClass(this, typ, AccessFlag.getAccessFlags(classAccessFlag))
    addNeedToResolveExtend(clazz, JAVA_TOPLEVEL_OBJECT_TYPE)
    if(isInnerClass(typ)) addNeedToResolveOuterClass(clazz, getOuterTypeFrom(typ))
    clazz.setResolvingLevel(ResolveLevel.BODY)
    addClass(clazz)
    clazz.addField(createClassField(clazz))
    val field: JawaField = JawaField(clazz, "length", PrimitiveType("int"), AccessFlag.getAccessFlags("FINAL"))
	  resolveClassesRelationWholeProgram
  }
    
  /**
   * resolve all the classes, fields and procedures from symbol table producer which are provided from symbol table model
   */
	def resolveFromST(st: CompilationUnitSymbolTable, level: ResolveLevel.Value, par: Boolean): Unit = {
	  resolveClasses(st.classOrInterfaceSymbolTables, level, par)
	  if(DEBUG){
	    printDetails
	  }
	}
	
	/**
	 * collect class info from symbol table
	 */
	def resolveClasses(cists: Iterable[ClassOrInterfaceSymbolTable], level: ResolveLevel.Value, par: Boolean) = {
	  if(DEBUG) println("Doing " + TITLE + ". Resolve classes parallel: " + par)
	  val col: GenIterable[ClassOrInterfaceSymbolTable] = if(par) cists.par else cists
	  val classes = col.map{
	    cit =>
        val cid = cit.classOrInterfaceDecl
	      val classType = cit.classOrInterfaceType
	      val recAccessFlag =	cid.accessModifier				// It can be PUBLIC ... or empty (which means no access flag class)
	      val clazz: JawaClass = new JawaClass(this, classType, recAccessFlag)
	      val exs = cid.parents
	      if(!exs.isEmpty) addNeedToResolveExtends(clazz, exs.toSet)
	      if(isInnerClass(clazz.getType)) addNeedToResolveOuterClass(clazz, getOuterTypeFrom(clazz.getType))
        clazz.setAST(cid)
	      resolveFields(clazz, cit.fieldDecls, par)
        resolveMethods(clazz, cit.methodSymbolTables, level, par)
	      clazz.setResolvingLevel(level)
	      clazz
	  }.toSet
	  resolveClassesRelationWholeProgram
	  // now we generate a special Jawa Method for each class; this proc would represent the const-class operation
	  classes.foreach{
	    clazz =>
	      clazz.addField(createClassField(clazz))
	  }
	}
	
	private def createClassField(rec: JawaClass): JawaField = {
	  JawaField(rec, "class", new ObjectType("java.lang.Class"), AccessFlag.getAccessFlags("FINAL_STATIC"))
	}
	
	/**
	 * collect global variables info from the symbol table
	 */
	def resolveFields(declaringClass: JawaClass, fieldDecls: Iterable[Field with Declaration], par: Boolean) = {
	  if(DEBUG) println("Doing " + TITLE + ". Resolve field parallel: " + par)
	  val col: GenIterable[Field with Declaration] = if(par) fieldDecls.par else fieldDecls
	  col.foreach{
	    fieldDecl =>
	      val fieldName: String = fieldDecl.nameID.text // e.g. serialVersionUID
	      val accessFlags: Int = AccessFlag.getAccessFlags(fieldDecl.accessModifier)
	      val fieldType: JawaType = fieldDecl.typ.typ	      
	      JawaField(declaringClass, fieldName, fieldType, accessFlags)
	  }
	}
	
	/**
	 * collect method info from symbol table
	 */
	def resolveMethods(declaringClass: JawaClass, msts: Iterable[MethodSymbolTable], level: ResolveLevel.Value, par: Boolean) = {
	  if(DEBUG) println("Doing " + TITLE + ". Resolve methods parallel: " + par)
	  val col: GenIterable[MethodSymbolTable] = if(par) msts.par else msts
	  col.foreach{
	    mst =>
        val md: MethodDeclaration = mst.methodDecl
	      val methodName: String = mst.methodName
	      val methodSignature: Signature = mst.methodSig
	      val accessFlags: Int = AccessFlag.getAccessFlags(md.accessModifier)
        val thisOpt: Option[String] = mst.thisNameOpt
        val params: ISeq[(String, JawaType)] = mst.params.map{p => (p.name, p.typ.typ)}
        val returnType: JawaType = md.returnType.typ
	      val method: JawaMethod = JawaMethod(declaringClass, methodName, thisOpt, params, returnType, accessFlags)
	      method.setResolvingLevel(level)
        method.setAST(md)
	      if(level >= ResolveLevel.BODY){
	      	if(md.body.isInstanceOf[ResolvedBody]){
		        val body = md.body.asInstanceOf[ResolvedBody]
		        val catchclauses = body.catchClauses
		        catchclauses.foreach{
		          catchclause =>
		            val excName = catchclause.typOrAny.fold(_.typ.asInstanceOf[ObjectType].name, _.text)
			          method.addExceptionHandler(excName, catchclause.from, catchclause.to, catchclause.goto.text)
		        }
		      }
	      }
	  }
	}
  
}
