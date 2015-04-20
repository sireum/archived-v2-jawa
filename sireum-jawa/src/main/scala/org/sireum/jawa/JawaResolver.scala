/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa

import org.sireum.pilar.symbol.SymbolTableProducer
import org.sireum.pilar.ast._
import org.sireum.util._
import scala.collection.GenMap
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.jawa.util.StringFormConverter
import org.sireum.pilar.symbol.SymbolTable
import org.sireum.jawa.MessageCenter._
import scala.collection.Parallel

/**
 * this object collects info from the symbol table and builds Center, JawaClass, and JawaMethod
 *
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 *
 */
object JawaResolver {
  
  val DEBUG : Boolean = false
  private final val TITLE : String = "JawaResolver"
  
  /**
   * resolve the given method code. Normally only for dummyMain i.e. environment method
   */
    
  def resolveMethodCode(procSig : String, code : String) : JawaMethod = {
    val st = Transform.getSymbolResolveResult(Set(code))
    resolveFromST(st, Center.ResolveLevel.BODY, GlobalConfig.jawaResolverParallel)
    Center.getMethodWithoutFailing(procSig)
  }
  
  /**
   * resolve the given method's body to body level. 
   */
    
  def resolveMethodBody(procSig : String) : MethodBody = {
    val code = JawaCodeSource.getMethodCodeWithoutFailing(procSig)
    val st = Transform.getSymbolResolveResult(Set(code))
    st.procedureSymbolTables.foreach{
      pst =>
        val sig = 
	        pst.procedure.getValueAnnotation("signature") match {
			      case Some(exp : NameExp) =>
			        exp.name.name
			      case _ => throw new RuntimeException("Doing " + TITLE + ": Can not find signature from: " + pst.procedureUri)
			    }
        if(procSig == sig) return pst.asInstanceOf[MethodBody]
    }
    throw new RuntimeException("Doing " + TITLE + ": Can not resolve procedure body for " + procSig)
  }
    
  /**
   * resolve the given classes to desired level. 
   */
    
  def tryResolveClass(className : String, desiredLevel : Center.ResolveLevel.Value) : Option[JawaClass] = {
    if(JawaCodeSource.containsClass(className)){
	    val r = desiredLevel match{
	      case Center.ResolveLevel.BODY => resolveToBody(className)
	      case Center.ResolveLevel.HIERARCHY => resolveToHierarchy(className)
	    }
	    Some(r)
    } else {
      None
    }
  }
    
  /**
   * resolve the given classes to desired level. 
   */
    
  def resolveClass(className : String, desiredLevel : Center.ResolveLevel.Value) : JawaClass = {
    val typ = StringFormConverter.getTypeFromName(className)
    if(!typ.isArray && !JawaCodeSource.containsClass(className)){
      if(!Center.containsClass(className) || Center.getClass(className).getResolvingLevel < desiredLevel){
	      val rec = new JawaClass().init(className)
	      rec.setUnknown
	      rec.setResolvingLevel(desiredLevel)
	      Center.tryRemoveClass(className)
	      Center.addClass(rec)
	      msg_detail(TITLE, "add phantom class " + rec)
	      rec
      } else Center.getClass(className)
    } else {
	    desiredLevel match{
	      case Center.ResolveLevel.BODY => resolveToBody(className)
	      case Center.ResolveLevel.HIERARCHY => resolveToHierarchy(className)
	    }
    }
  }
  
  /**
   * resolve the given classes to desired level. 
   */
    
  def forceResolveClass(className : String, desiredLevel : Center.ResolveLevel.Value) : JawaClass = {
    desiredLevel match{
      case Center.ResolveLevel.BODY => forceResolveToBody(className)
      case Center.ResolveLevel.HIERARCHY => forceResolveToHierarchy(className)
    }
  }
  
  /**
   * resolve the given class to hierarchy level
   */
  
  def resolveToHierarchy(className : String) : JawaClass = {
    if(!Center.containsClass(className) || Center.getClass(className).getResolvingLevel < Center.ResolveLevel.HIERARCHY) forceResolveToHierarchy(className)
    Center.getClass(className)
  }
  
  /**
   * force resolve the given class to hierarchy level
   */
  
  private def forceResolveToHierarchy(className : String) : JawaClass = {
    val typ = StringFormConverter.getTypeFromName(className)
    if(typ.isArray){
      resolveArrayClass(typ)
    } else {
	    val code = JawaCodeSource.getClassCode(className, Center.ResolveLevel.HIERARCHY)
	    val st = Transform.getSymbolResolveResult(Set(code))
	    Center.tryRemoveClass(className)
	    resolveFromST(st, Center.ResolveLevel.HIERARCHY, GlobalConfig.jawaResolverParallel)
    }
    Center.getClass(className)
  }
  
  /**
   * resolve the given class to body level
   */
  
  def resolveToBody(className : String) : JawaClass = {
    if(!Center.containsClass(className)) forceResolveToBody(className)
    else if(Center.getClass(className).getResolvingLevel < Center.ResolveLevel.BODY) escalateReolvingLevel(Center.getClass(className), Center.ResolveLevel.BODY)
    else Center.getClass(className)
  }
  
  /**
   * escalate resolving level
   */
  
  private def escalateReolvingLevel(rec : JawaClass, desiredLevel : Center.ResolveLevel.Value) : JawaClass = {
    require(rec.getResolvingLevel < desiredLevel)
    if(desiredLevel == Center.ResolveLevel.BODY){
      rec.getMethods.foreach(_.tryResolveBody)
      rec.setResolvingLevel(Center.ResolveLevel.BODY)
    }
    rec
  }
  
  /**
   * force resolve the given class to body level
   */
  
  private def forceResolveToBody(className : String) : JawaClass = {
    val typ = StringFormConverter.getTypeFromName(className)
    if(typ.isArray){
      resolveArrayClass(typ)
    } else {
	    val code = JawaCodeSource.getClassCode(className, Center.ResolveLevel.BODY)
	    val st = Transform.getSymbolResolveResult(Set(code))
	    Center.tryRemoveClass(className)
	    resolveFromST(st, Center.ResolveLevel.BODY, GlobalConfig.jawaResolverParallel)
    }
    Center.getClass(className)
  }
  
  /**
   * resolve array class
   */
  
  def resolveArrayClass(typ : Type) : Unit = {
    val recName = typ.name
    val recAccessFlag =	
      if(Center.isJavaPrimitiveType(typ.typ)){
      	"FINAL_PUBLIC"
	    } else {
	      val base = resolveClass(typ.typ, Center.ResolveLevel.HIERARCHY)
	      val baseaf = base.getAccessFlagString
	      if(baseaf.contains("FINAL")) baseaf else "FINAL_" + baseaf
	    }
    val rec : JawaClass = new JawaClass
    rec.init(recName)
    rec.setAccessFlags(recAccessFlag)
    rec.addNeedToResolveExtends(Set(Center.DEFAULT_TOPLEVEL_OBJECT))
    if(Center.isInnerClassName(recName)) rec.needToResolveOuterName = Some(Center.getOuterNameFrom(recName))
    rec.setResolvingLevel(Center.ResolveLevel.BODY)
    Center.addClass(rec)
    rec.addField(createClassField(rec))
    val field : JawaField = new JawaField
    val fSig = StringFormConverter.generateFieldSignature(rec.getName, "length", false)
    field.init(fSig, NormalType("int", 0))
    field.setAccessFlags("FINAL")
    rec.addField(field)
	  Center.resolveClassesRelationWholeProgram
  }
    
  /**
   * resolve all the classes, fields and procedures from symbol table producer which are provided from symbol table model
   */
	
	def resolveFromST(st : SymbolTable, level : Center.ResolveLevel.Value, par : Boolean) : Unit = {
    if(!JawaCodeSource.isPreLoaded) throw new RuntimeException("In whole program mode but library code did not been pre-loaded, call JawaCodeSource.preLoad first.")
    val stp = st.asInstanceOf[SymbolTableProducer]
	  resolveClasses(stp, level, par)
	  resolveGlobalVars(stp, level, par)
	  resolveMethods(stp, level, par)
	  if(DEBUG){
	    Center.printDetails
	  }
	}
	
	/**
	 * collect class info from symbol table
	 */
	
	def resolveClasses(stp : SymbolTableProducer, level : Center.ResolveLevel.Value, par : Boolean) = {
	  if(DEBUG) println("Doing " + TITLE + ". Resolve classes parallel: " + par)
	  val col : GenMap[ResourceUri, RecordDecl] = if(par) stp.tables.recordTable.par else stp.tables.recordTable
	  val classes = col.map{
	    case (uri, rd) =>
	      val recName = rd.name.name
	      val recAccessFlag =					// It can be PUBLIC ... or empty (which means no access flag class)
	        rd.getValueAnnotation("AccessFlag") match {
            case Some(exp : NameExp) =>
              exp.name.name
            case _ => ""
          }
	      val rec : JawaClass = new JawaClass
	      rec.init(recName)
	      rec.setAccessFlags(recAccessFlag)
	      var exs = rd.extendsClauses.map {_.name.name}.toSet
	      rec.addNeedToResolveExtends(exs)
	      if(Center.isInnerClassName(recName)) rec.needToResolveOuterName = Some(Center.getOuterNameFrom(recName))
	      rd.attributes.foreach{
	        field =>
	          val fieldSig = field.name.name
	          val fieldAccessFlag =					// It can be PRIVATE ...
			        rd.getValueAnnotation("AccessFlag") match {
		            case Some(exp : NameExp) =>
		              exp.name.name
		            case _ => ""
		          }
	          require(field.typeSpec.isDefined)
	          var d = 0
			      var tmpTs = field.typeSpec.get
			      while(tmpTs.isInstanceOf[SeqTypeSpec]){
		          d += 1
		          tmpTs = tmpTs.asInstanceOf[SeqTypeSpec].elementType
		        }
			      require(tmpTs.isInstanceOf[NamedTypeSpec])
			      val fieldType : NormalType = new NormalType(tmpTs.asInstanceOf[NamedTypeSpec].name.name, d)
	          val f : JawaField = new JawaField
	          f.init(fieldSig, fieldType)
	          f.setAccessFlags(fieldAccessFlag)
	          rec.addField(f)
	      }
	      rec.setResolvingLevel(level)
	      rec
	  }.toSet
	  classes.foreach(Center.addClass(_))
	  Center.resolveClassesRelationWholeProgram
//	  else Center.resolveClassesRelation
	  // now we generate a special Amandroid Method for each class; this proc would represent the const-class operation
	  classes.foreach{
	    rec =>
	      rec.addField(createClassField(rec))
	  }
	}
	
	private def createClassField(rec : JawaClass) : JawaField = {
	  val field : JawaField = new JawaField
    val fSig = StringFormConverter.generateFieldSignature(rec.getName, "class", false)
    field.init(fSig, NormalType("java.lang.Class", 0))
    field.setAccessFlags("FINAL_STATIC")
    field
	}
	
	/**
	 * collect global variables info from the symbol table
	 */
	
	def resolveGlobalVars(stp : SymbolTableProducer, level : Center.ResolveLevel.Value, par : Boolean) = {
	  if(DEBUG) println("Doing " + TITLE + ". Resolve global variables parallel: " + par)
	  val col : GenMap[ResourceUri, GlobalVarDecl] = if(par) stp.tables.globalVarTable.par else stp.tables.globalVarTable
	  val ownerRelation = col.map{
	    case (uri, gvd) =>
	      val globalVarSig = gvd.name.name // e.g. @@java.lang.Enum.serialVersionUID
	      import org.sireum.pilar.symbol.Symbol.pp2r
	      Center.setGlobalVarSigToUri(gvd.name.name, gvd.name.uri)
	      val globalVarAccessFlag = 
	        gvd.getValueAnnotation("AccessFlag") match {
			      case Some(exp : NameExp) =>
			        exp.name.name
			      case _ => ""
			    }
	      require(gvd.typeSpec.isDefined)
	      var d = 0
	      var tmpTs = gvd.typeSpec.get
	      while(tmpTs.isInstanceOf[SeqTypeSpec]){
          d += 1
          tmpTs = tmpTs.asInstanceOf[SeqTypeSpec].elementType
        }
	      require(tmpTs.isInstanceOf[NamedTypeSpec])
	      val globalVarType : NormalType = new NormalType(tmpTs.asInstanceOf[NamedTypeSpec].name.name, d)
	      val ownerName = StringFormConverter.getClassNameFromFieldSignature(globalVarSig)
	      
	      val f : JawaField = new JawaField
	      f.init(globalVarSig, globalVarType)
	      f.setAccessFlags(globalVarAccessFlag)
	      val ownerClass = Center.getClass(ownerName)
	      (f, ownerClass)
	  }
	  if(ownerRelation.isInstanceOf[Parallel]) throw new RuntimeException("Doing " + TITLE + ": ownerRelation is parallel, but we are trying to add things to JawaClass.")
	  ownerRelation.foreach{
	    case (f, own) =>
	      own.addField(f)
	  }
	}
	
	/**
	 * collect method info from symbol table
	 */
	
	def resolveMethods(stp : SymbolTableProducer, level : Center.ResolveLevel.Value, par : Boolean) = {
	  if(DEBUG) println("Doing " + TITLE + ". Resolve methods parallel: " + par)
	  val col : GenMap[ResourceUri, ProcedureDecl] = if(par) stp.tables.procedureAbsTable.par else stp.tables.procedureAbsTable
	  val ownerRelation = col.map{
	    case (uri, pd) =>
	      val procName = pd.name.name
	      val procSig = 
	        pd.getValueAnnotation("signature") match {
			      case Some(exp : NameExp) =>
			        exp.name.name
			      case _ => throw new RuntimeException("Doing " + TITLE + ": Can not find signature from: " + procName)
			    }
	      val procAccessFlag = 
	        pd.getValueAnnotation("Access") match {
			      case Some(exp : NameExp) =>
			        exp.name.name
			      case _ => ""
			    }
	      val ownerName =
          pd.getValueAnnotation("owner") match {
            case Some(exp : NameExp) => 
              exp.name.name
            case _ => throw new RuntimeException("Doing " + TITLE + ": Can not find owner from: " + procName)
          }
	      val paramNames = pd.params.map{_.name.name}.toList
	      val proc : JawaMethod = new JawaMethod
	      proc.init(procName, procSig)
	      proc.setAccessFlags(procAccessFlag)
	      proc.setParameterNames(paramNames)
	      proc.setResolvingLevel(level)
	      val ownerClass = Center.getClass(ownerName)
	      if(level >= Center.ResolveLevel.BODY){
	      	proc.setMethodBody(stp.procedureSymbolTableProducer(uri).asInstanceOf[MethodBody])
		      if(pd.body.isInstanceOf[ImplementedBody]){
		        val body = pd.body.asInstanceOf[ImplementedBody]
		        val catchclauses = body.catchClauses
		        catchclauses.foreach{
		          catchclause =>
		            require(catchclause.typeSpec.isDefined)
		            require(catchclause.typeSpec.get.isInstanceOf[NamedTypeSpec])
		            val excName = catchclause.typeSpec.get.asInstanceOf[NamedTypeSpec].name.name
			          proc.addExceptionHandler(excName, catchclause.fromTarget.name, catchclause.toTarget.name, catchclause.jump.target.name)
		        }
		      }
	      }
	      (proc, ownerClass)
	  }
	  if(ownerRelation.isInstanceOf[Parallel]) throw new RuntimeException("Doing " + TITLE + ": ownerRelation is parallel, but we are trying to add things to JawaClass.")
	  ownerRelation.foreach{
	    case (proc, own) =>
	      own.addMethod(proc)
	  }
	}
	
}
