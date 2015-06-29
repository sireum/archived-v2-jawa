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
import org.sireum.pilar.symbol.SymbolTable
import scala.collection.Parallel
import org.sireum.jawa.io.SourceFile
import org.sireum.jawa.symbolResolver.JawaSymbolTable
import org.sireum.pilar.parser.Parser
import org.sireum.jawa.symbolResolver.JawaSymbolTableBuilder
import org.sireum.jawa.io.NoPosition
import org.sireum.jawa.io.AbstractFile

/**
 * this object collects info from the symbol table and builds Center, JawaClass, and JawaMethod
 *
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait JawaResolver extends JawaClasspathManager with JavaKnowledge {self: Global =>
  
  val DEBUG: Boolean = false
  private final val TITLE: String = "JawaResolver"
  
  var fst = { _: Unit => new JawaSymbolTable }
  
  def parseCodes(codes: Set[String]): Model = {
    val sb = new StringBuilder
    codes.foreach{
      code => sb.append(code + "\n")
    }
    val (modelopt, err) = Parser.parseWithErrorAsString[Model](Left(sb.toString)) 
    modelopt match{case Some(m) => m; case None => throw new RuntimeException(err + "\n" + sb.toString)}
  }
  
  def getSymbolResolveResult(codes: Set[String]): SymbolTable = {
    val newModel = parseCodes(codes)
    JawaSymbolTableBuilder(List(newModel), fst, true)
  }
  
  /**
   * resolve the given method code. Normally only for dummyMain i.e. environment method
   */
  def resolveMethodCode(procSig: Signature, code: String): JawaMethod = {
    val st = getSymbolResolveResult(Set(code))
    resolveFromST(st, ResolveLevel.BODY, true)
    getMethod(procSig).get
  }
    
  /**
   * resolve the given classes to desired level. 
   */
  def resolveClass(classType: ObjectType, desiredLevel: ResolveLevel.Value): JawaClass = {
    if(!classType.isArray && !containsClassFile(classType)){
      if(!containsClass(classType) || getClass(classType).get.getResolvingLevel < desiredLevel){
	      val rec = new JawaClass(this, classType, "")
	      rec.setUnknown
	      rec.setResolvingLevel(desiredLevel)
	      reporter.echo(NoPosition, TITLE + " add phantom class " + rec)
	      rec
      } else getClass(classType).get
    } else {
	    desiredLevel match{
	      case ResolveLevel.BODY => resolveToBody(classType)
	      case ResolveLevel.HIERARCHY => resolveToHierarchy(classType)
	    }
    }
  }
  
  /**
   * resolve the given classes to desired level. 
   */
  def forceResolveClass(classType: ObjectType, desiredLevel: ResolveLevel.Value): JawaClass = {
    desiredLevel match{
      case ResolveLevel.BODY => forceResolveToBody(classType)
      case ResolveLevel.HIERARCHY => forceResolveToHierarchy(classType)
    }
  }
  
  def getClassCode(file: AbstractFile, level: ResolveLevel.Value) : String = {
    var code: String = file.text
    if(level < ResolveLevel.BODY){
      code = LightWeightPilarParser.getEmptyBodyCode(code)
    }
    code
  }
  
  /**
   * resolve the given class to hierarchy level
   */
  def resolveToHierarchy(classType: ObjectType): JawaClass = {
    if(!containsClass(classType) || getClass(classType).get.getResolvingLevel < ResolveLevel.HIERARCHY) forceResolveToHierarchy(classType)
    getClass(classType).get
  }
  
  /**
   * force resolve the given class to hierarchy level
   */
  private def forceResolveToHierarchy(classType: ObjectType): JawaClass = {
    if(classType.isArray){
      resolveArrayClass(classType)
    } else {
	    val file = getClassFile(classType)
      val code = getClassCode(file, ResolveLevel.HIERARCHY)
	    val st = getSymbolResolveResult(Set(code))
	    removeClass(classType)
	    resolveFromST(st, ResolveLevel.HIERARCHY, true)
    }
    getClass(classType).get
  }
  
  /**
   * resolve the given class to body level
   */
  def resolveToBody(classType: ObjectType): JawaClass = {
    if(!containsClass(classType)) forceResolveToBody(classType)
    else if(getClass(classType).get.getResolvingLevel < ResolveLevel.BODY) escalateReolvingLevel(getClass(classType).get, ResolveLevel.BODY)
    else getClass(classType).get
  }
  
  /**
   * escalate resolving level
   */
  private def escalateReolvingLevel(rec: JawaClass, desiredLevel: ResolveLevel.Value): JawaClass = {
    require(rec.getResolvingLevel < desiredLevel)
    if(desiredLevel == ResolveLevel.BODY){
      rec.getMethods.foreach(_.tryResolveBody)
      rec.setResolvingLevel(ResolveLevel.BODY)
    }
    rec
  }
  
  /**
   * force resolve the given class to body level
   */
  private def forceResolveToBody(classType: ObjectType): JawaClass = {
    if(classType.isArray){
      resolveArrayClass(classType)
    } else {
	    val file = getClassFile(classType)
      val code = getClassCode(file, ResolveLevel.BODY)
	    val st = getSymbolResolveResult(Set(code))
	    removeClass(classType)
	    resolveFromST(st, ResolveLevel.BODY, true)
    }
    getClass(classType).get
  }
  
  /**
   * resolve array class
   */
  def resolveArrayClass(typ: ObjectType): Unit = {
    val recAccessFlag =	
      if(isJavaPrimitive(typ.typ)){
      	"FINAL_PUBLIC"
	    } else {
	      val base = resolveClass(typ, ResolveLevel.HIERARCHY)
	      val baseaf = base.getAccessFlagsStr
	      if(baseaf.contains("FINAL")) baseaf else "FINAL_" + baseaf
	    }
    val rec: JawaClass = new JawaClass(this, typ, recAccessFlag)
    addNeedToResolveExtends(rec, Set(JAVA_TOPLEVEL_OBJECT_TYPE))
    if(isInnerClass(typ)) addNeedToResolveOuterClass(rec, getOuterTypeFrom(typ))
    rec.setResolvingLevel(ResolveLevel.BODY)
    rec.addField(createClassField(rec))
    val field: JawaField = new JawaField(rec, "length", PrimitiveType("int"), "FINAL")
	  resolveClassesRelationWholeProgram
  }
    
  /**
   * resolve all the classes, fields and procedures from symbol table producer which are provided from symbol table model
   */
	def resolveFromST(st: SymbolTable, level: ResolveLevel.Value, par: Boolean): Unit = {
    val stp = st.asInstanceOf[SymbolTableProducer]
	  resolveClasses(stp, level, par)
	  resolveGlobalVars(stp, level, par)
	  resolveMethods(stp, level, par)
	  if(DEBUG){
	    printDetails
	  }
	}
	
	/**
	 * collect class info from symbol table
	 */
	def resolveClasses(stp: SymbolTableProducer, level: ResolveLevel.Value, par: Boolean) = {
	  if(DEBUG) println("Doing " + TITLE + ". Resolve classes parallel: " + par)
	  val col: GenMap[ResourceUri, RecordDecl] = if(par) stp.tables.recordTable.par else stp.tables.recordTable
	  val classes = col.map{
	    case (uri, rd) =>
	      val recTyp = getTypeFromName(rd.name.name).asInstanceOf[ObjectType]
	      val accessFlag =					// It can be PUBLIC ... or empty (which means no access flag class)
	        rd.getValueAnnotation("AccessFlag") match {
            case Some(exp: NameExp) =>
              exp.name.name
            case _ => ""
          }
	      val rec: JawaClass = new JawaClass(this, recTyp, accessFlag)
	      val exs = rd.extendsClauses.map {_.name.name}.map(getTypeFromName(_).asInstanceOf[ObjectType]).toSet
	      addNeedToResolveExtends(rec, exs)
	      if(isInnerClass(recTyp)) addNeedToResolveOuterClass(rec, getOuterTypeFrom(recTyp))
	      rd.attributes.foreach{
	        field =>
	          val fieldSig = field.name.name
	          val fieldAccessFlag =					// It can be PRIVATE ...
			        rd.getValueAnnotation("AccessFlag") match {
		            case Some(exp: NameExp) =>
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
            val fieldName: String = getFieldNameFromFieldFQN(fieldSig)
			      val fieldType: JawaType = JawaType.generateType(tmpTs.asInstanceOf[NamedTypeSpec].name.name, d)
	          new JawaField(rec, fieldName, fieldType, fieldAccessFlag)
	      }
	      rec.setResolvingLevel(level)
	      rec
	  }.toSet
	  Center.resolveClassesRelationWholeProgram
//	  else Center.resolveClassesRelation
	  // now we generate a special Amandroid Method for each class; this proc would represent the const-class operation
	  classes.foreach{
	    rec =>
	      rec.addField(createClassField(rec))
	  }
	}
	
	private def createClassField(rec: JawaClass): JawaField = {
	  new JawaField(rec, "class", ObjectType("java.lang.Class", 0), "FINAL_STATIC")
	}
	
	/**
	 * collect global variables info from the symbol table
	 */
	def resolveGlobalVars(stp: SymbolTableProducer, level: ResolveLevel.Value, par: Boolean) = {
	  if(DEBUG) println("Doing " + TITLE + ". Resolve global variables parallel: " + par)
	  val col: GenMap[ResourceUri, GlobalVarDecl] = if(par) stp.tables.globalVarTable.par else stp.tables.globalVarTable
	  col.map{
	    case (uri, gvd) =>
	      val globalVarSig = gvd.name.name.replaceAll("@@", "") // e.g. @@java.lang.Enum.serialVersionUID
	      val globalVarAccessFlag = 
	        gvd.getValueAnnotation("AccessFlag") match {
			      case Some(exp: NameExp) =>
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
        val globalVarName: String = getFieldNameFromFieldFQN(globalVarSig)
	      val globalVarType: JawaType = JawaType.generateType(tmpTs.asInstanceOf[NamedTypeSpec].name.name, d)
	      val ownerType = getClassTypeFromFieldFQN(globalVarSig)
	      val ownerClass = getClass(ownerType).get
	      new JawaField(ownerClass, globalVarName, globalVarType, globalVarAccessFlag)
	  }
	}
	
	/**
	 * collect method info from symbol table
	 */
	def resolveMethods(stp: SymbolTableProducer, level: ResolveLevel.Value, par: Boolean) = {
	  if(DEBUG) println("Doing " + TITLE + ". Resolve methods parallel: " + par)
	  val col: GenMap[ResourceUri, ProcedureDecl] = if(par) stp.tables.procedureAbsTable.par else stp.tables.procedureAbsTable
	  col.map{
	    case (uri, pd) =>
	      val procName = pd.name.name
	      val procSig = 
	        pd.getValueAnnotation("signature") match {
			      case Some(exp: NameExp) =>
			        Signature(exp.name.name)
			      case _ => throw new RuntimeException("Doing " + TITLE + ": Can not find signature from: " + procName)
			    }
	      val procAccessFlag = 
	        pd.getValueAnnotation("Access") match {
			      case Some(exp: NameExp) =>
			        exp.name.name
			      case _ => ""
			    }
        val ownerClass: JawaClass = getClass(procSig.getClassType).get
        val thisOpt: Option[String] = {
          pd.params.find { x => x.getValueAnnotation("kind") match {
            case Some(n: NameExp) => n.name.name == "this"
            case _ => false
          } }.map(_.name.name)
        }
	      val paramNames = pd.params.map{_.name.name}.toList
        val paramTypes = procSig.getParameterTypes()
        val paramsize = paramNames.size
        val params: MList[(String, JawaType)] = mlistEmpty
        for(i <- 0 to paramsize - 1){
          params(i) = ((paramNames(i), paramTypes(i)))
        }
        
	      val proc: JawaMethod = new JawaMethod(ownerClass, procSig.methodNamePart, thisOpt, params.toList, procSig.getReturnType(), AccessFlag.getAccessFlags(procAccessFlag))
	      proc.setResolvingLevel(level)
	      
	      if(level >= ResolveLevel.BODY){
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
	}
	
}
