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
import org.sireum.jawa.sjc.Transform
import org.sireum.jawa.sjc.ResolveLevel
import org.sireum.jawa.sjc.Signature
import org.sireum.jawa.sjc.JavaKnowledge
import org.sireum.jawa.sjc.symtab.CompilationUnitSymbolTable
import org.sireum.jawa.sjc.symtab.CompilationUnitSymbolTableProducer
import org.sireum.jawa.sjc.ObjectType
import org.sireum.jawa.sjc.PrimitiveType
import org.sireum.jawa.sjc.AccessFlag
import org.sireum.jawa.sjc.symtab.ClassOrInterfaceSymbolTable
import org.sireum.jawa.sjc.parser.ClassOrInterfaceDeclaration
import scala.collection.GenIterable
import org.sireum.jawa.sjc.parser._
import org.sireum.jawa.sjc.JawaType
import org.sireum.jawa.sjc.symtab.MethodSymbolTable
import org.sireum.jawa.sjc.symtab.MethodSymbolTable

/**
 * this object collects info from the symbol table and builds Global, JawaClass, and JawaMethod
 *
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait JawaResolver extends JavaKnowledge with ResolveLevel {self: Global =>
  
  val DEBUG: Boolean = false
  private final val TITLE: String = "JawaResolver"
  
  /**
   * resolve the given method code. Normally only for dummyMain i.e. environment method
   */
//  def resolveMethodCode(signature: Signature, code: String): JawaMethod = {
//    val st = Transform.getSymbolResolveResult(Set(code))
//    resolveFromST(st, ResolveLevel.BODY, true)
//    getMethodWithoutFailing(signature)
//  }
  
  /**
   * resolve the given method's body to body level. 
   */
//  def resolveMethodBody(signature: Signature): MethodSymbolTable = {
//    val code = JawaCodeSource.getMethodCodeWithoutFailing(signature)
//    val st = Transform.getSymbolResolveResult(Set(code))
//    st.methodSymbolTables.foreach{
//      pst =>
//        val sig = 
//	        pst.procedure.getValueAnnotation("signature") match {
//			      case Some(exp: NameExp) =>
//			        exp.name.name
//			      case _ => throw new RuntimeException("Doing " + TITLE + ": Can not find signature from: " + pst.procedureUri)
//			    }
//        if(signature == sig) return pst.asInstanceOf[MethodBody]
//    }
//    throw new RuntimeException("Doing " + TITLE + ": Can not resolve procedure body for " + signature)
//  }
    
  /**
   * resolve the given classes to desired level. 
   */
  def tryResolveClass(typ: ObjectType, desiredLevel: ResolveLevel.Value): Option[JawaClass] = {
    if(JawaCodeSource.containsClass(typ)){
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
    if(!typ.isArray && !JawaCodeSource.containsClass(typ)){
      if(!containsClass(typ) || getClass(typ).getResolvingLevel < desiredLevel){
	      val rec = JawaClass(this, typ, 0)
	      rec.setUnknown
	      rec.setResolvingLevel(desiredLevel)
	      tryRemoveClass(typ)
	      addClass(rec)
//	      msg_detail(TITLE, "add phantom class " + rec)
	      rec
      } else getClass(typ)
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
    if(!containsClass(typ) || getClass(typ).getResolvingLevel < ResolveLevel.HIERARCHY) forceResolveToHierarchy(typ)
    getClass(typ)
  }
  
  /**
   * force resolve the given class to hierarchy level
   */
  private def forceResolveToHierarchy(typ: ObjectType): JawaClass = {
    if(typ.isArray){
      resolveArrayClass(typ)
    } else {
	    val code = JawaCodeSource.getClassCode(typ, ResolveLevel.HIERARCHY)
	    val st = Transform.getCompilationUnitSymbolResult(this, List((Some(code._1), code._2)))
	    tryRemoveClass(typ)
	    resolveFromST(st, ResolveLevel.HIERARCHY, true)
    }
    getClass(typ)
  }
  
  /**
   * resolve the given class to body level
   */
  def resolveToBody(typ: ObjectType): JawaClass = {
    if(!containsClass(typ)) forceResolveToBody(typ)
    else if(getClass(typ).getResolvingLevel < ResolveLevel.BODY) escalateReolvingLevel(getClass(typ), ResolveLevel.BODY)
    else getClass(typ)
  }
  
  /**
   * escalate resolving level
   */
  private def escalateReolvingLevel(rec: JawaClass, desiredLevel: ResolveLevel.Value): JawaClass = {
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
	    val code = JawaCodeSource.getClassCode(typ, ResolveLevel.BODY)
	    val st = Transform.getCompilationUnitSymbolResult(this, List((Some(code._1), code._2)))
	    tryRemoveClass(typ)
	    resolveFromST(st, ResolveLevel.BODY, true)
    }
    getClass(typ)
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
	      val baseaf = base.getAccessFlagString
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
	      val classType = new ObjectType(cit.classOrInterfaceName)
	      val recAccessFlag =	cid.accessModifier				// It can be PUBLIC ... or empty (which means no access flag class)
	      val clazz: JawaClass = new JawaClass(this, classType, recAccessFlag)
	      val exs = cid.parents.map(getTypeFromName(_).asInstanceOf[ObjectType]).toSet
	      if(!exs.isEmpty) addNeedToResolveExtends(clazz, exs)
	      if(isInnerClass(clazz.getType)) addNeedToResolveOuterClass(clazz, getOuterTypeFrom(clazz.getType))
	      resolveFields(clazz, cit.fieldDecls, par)
        resolveMethods(clazz, cit.methodSymbolTables, level, par)
	      clazz.setResolvingLevel(level)
	      clazz
	  }.toSet
	  classes.foreach(addClass(_))
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
	      val methodSignature: Signature = Signature(mst.methodSig)
	      val accessFlags: Int = AccessFlag.getAccessFlags(md.accessModifier)
        val thisOpt: Option[String] = mst.thisNameOpt
        val params: ISeq[(String, JawaType)] = mst.params.map{p => (p.name, p.typ.typ)}
        val returnType: JawaType = md.returnType.typ
	      val method: JawaMethod = JawaMethod(declaringClass, methodName, thisOpt, params, returnType, accessFlags)
	      method.setResolvingLevel(level)
	      if(level >= ResolveLevel.BODY){
//	      	proc.setMethodBody(stp.procedureSymbolTableProducer(uri).asInstanceOf[MethodBody])
//		      if(pd.body.isInstanceOf[ImplementedBody]){
//		        val body = pd.body.asInstanceOf[ImplementedBody]
//		        val catchclauses = body.catchClauses
//		        catchclauses.foreach{
//		          catchclause =>
//		            require(catchclause.typeSpec.isDefined)
//		            require(catchclause.typeSpec.get.isInstanceOf[NamedTypeSpec])
//		            val excName = catchclause.typeSpec.get.asInstanceOf[NamedTypeSpec].name.name
//			          proc.addExceptionHandler(excName, catchclause.fromTarget.name, catchclause.toTarget.name, catchclause.jump.target.name)
//		        }
//		      }
	      }
	  }
	}
	
}