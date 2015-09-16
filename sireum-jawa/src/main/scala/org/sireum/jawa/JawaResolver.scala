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
import org.sireum.jawa.sourcefile.SourcefileParser
import org.sireum.jawa.sourcefile.MySTVisitor

/**
 * this object collects info from the symbol table and builds Center, JawaClass, and JawaMethod
 *
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait JawaResolver extends JawaClasspathManager with JavaKnowledge {self: Global =>
  
  import JawaResolver._
  
  val DEBUG: Boolean = false
  private final val TITLE: String = "JawaResolver"
  
  /**
   * resolve the given method code. Normally only for dummyMain i.e. environment method
   */
  def resolveMethodCode(sig: Signature, code: String): JawaMethod = {
    val st = getSymbolResolveResult(Set(code))
    val v = new MySTVisitor
    val ms = v.resolveMethodOnly(st.asInstanceOf[SymbolTableProducer], ResolveLevel.BODY)
    val clazz = getClass(sig.getClassType) match {
      case Some(c) => c
      case None => resolveToBody(sig.getClassType)
    }
    ms foreach {
      m =>
        val method = resolveFromMyMethod(clazz, m)
        method.setBody(m.body.get) //here assume the body already resolved
    }
    getMethod(sig).get
  }
  
  /**
   * resolve the given method's body to body level. 
   */
  def resolveMethodBody(c: JawaClass) = {
    val typ = c.getType
    val mcs = this.applicationClassCodes.get(typ) match {
      case Some(asrc) =>
        SourcefileParser.parse(asrc, ResolveLevel.BODY)
      case None =>
        this.userLibraryClassCodes.get(typ) match {
          case Some(usrc) =>
            SourcefileParser.parse(usrc, ResolveLevel.BODY)
          case None =>
            reporter.error(NoPosition, "Could not find code for " + typ)
            throw new RuntimeException("Could not find code for " + typ)
        }
    }
    val mc = mcs(typ)
    mc.methods foreach {
      m =>
        c.getMethod(m.signature.getSubSignature) foreach(_.setBody(m.body.get))
    }
  }
    
  /**
   * resolve the given classes to desired level. 
   */
  protected[jawa] def resolveClass(classType: ObjectType, desiredLevel: ResolveLevel.Value, allowUnknown: Boolean): JawaClass = {
    val clazz =
      if(!classType.isArray && !containsClassFile(classType)) {
        if(!allowUnknown) throw JawaResolverError("Does not find class " + classType + " and don't allow unknown.")
        if(desiredLevel >= ResolveLevel.BODY) throw JawaResolverError("Does not allow unknown class " + classType + " resolve to body level.")
        getClass(classType) match {
          case None =>
            val rec = new JawaClass(this, classType, "")
            rec.setUnknown
            rec.setResolvingLevel(desiredLevel)
            reporter.echo(NoPosition, TITLE + " add phantom class " + rec)
            addClassNotFound(classType)
            rec
          case Some(c) =>
            c
        }
      } else {
        getClass(classType) match {
          case None =>
            desiredLevel match{
              case ResolveLevel.BODY => forceResolveToBody(classType)
              case ResolveLevel.HIERARCHY => forceResolveToHierarchy(classType)
            }
          case Some(c) =>
            if(c.getResolvingLevel < desiredLevel) escalateReolvingLevel(c, desiredLevel)
            else c
        }
      }
    resolveClassesRelationWholeProgram
    clazz
  }
  
  /**
   * resolve the given classes to desired level. 
   */
//  def forceResolveClass(classType: ObjectType, desiredLevel: ResolveLevel.Value): JawaClass = {
//    desiredLevel match{
//      case ResolveLevel.BODY => forceResolveToBody(classType)
//      case ResolveLevel.HIERARCHY => forceResolveToHierarchy(classType)
//    }
//  }
  
  protected[jawa] def getClassCode(file: AbstractFile, level: ResolveLevel.Value) : String = {
    var code: String = file.text
    if(level < ResolveLevel.BODY) {
      code = LightWeightPilarParser.getEmptyBodyCode(code)
    }
    code
  }
  
  /**
   * resolve the given class to hierarchy level
   */
  @throws(classOf[JawaResolverError])
  def resolveToHierarchy(classType: ObjectType, allowUnknown: Boolean = true): JawaClass = {
    resolveClass(classType, ResolveLevel.HIERARCHY, allowUnknown)
  }
  
  /**
   * force resolve the given class to hierarchy level
   */
  private def forceResolveToHierarchy(classType: ObjectType): JawaClass = {
    val clazz = if(classType.isArray){
      resolveArrayClass(classType)
    } else {
      val mc = getMyClass(classType).get
      removeClass(classType)
      resolveFromMyClass(mc)
    }
    clazz
  }
  
  /**
   * resolve the given class to body level. Unknown class cannot resolve to body level.
   * It will throw JawaResolverError if violate.
   */
  @throws(classOf[JawaResolverError])
  def resolveToBody(classType: ObjectType): JawaClass = {
    resolveClass(classType, ResolveLevel.BODY, false)
  }
  
  /**
   * escalate resolving level
   */
  private def escalateReolvingLevel(clazz: JawaClass, desiredLevel: ResolveLevel.Value): JawaClass = {
    require(clazz.getResolvingLevel < desiredLevel)
    if(desiredLevel == ResolveLevel.BODY){
      clazz.getMethods.foreach(m => m.getBody)
      clazz.setResolvingLevel(ResolveLevel.BODY)
    }
    clazz
  }
  
  /**
   * force resolve the given class to body level
   */
  private def forceResolveToBody(classType: ObjectType): JawaClass = {
    val clazz =
      if(classType.isArray){
        resolveArrayClass(classType)
      } else {
        val mc = getMyClass(classType).get
        removeClass(classType)
        resolveFromMyClass(mc)
      }
    clazz
  }
  
  /**
   * resolve array class
   */
  private def resolveArrayClass(typ: ObjectType): JawaClass = {
    val recAccessFlag =
      if(isJavaPrimitive(typ.typ)){
        "FINAL_PUBLIC"
      } else {
        val base = resolveClass(new ObjectType(typ.typ), ResolveLevel.HIERARCHY, true)
        val baseaf = base.getAccessFlagsStr
        if(baseaf.contains("FINAL")) baseaf else "FINAL_" + baseaf
      }
    val clazz: JawaClass = new JawaClass(this, typ, recAccessFlag)
    addNeedToResolveExtend(clazz, JAVA_TOPLEVEL_OBJECT_TYPE)
    clazz.setResolvingLevel(ResolveLevel.BODY)
    new JawaField(clazz, "class", new ObjectType("java.lang.Class"), "FINAL_STATIC")
    new JawaField(clazz, "length", PrimitiveType("int"), "FINAL")
    clazz
  }
    
  protected def resolveFromMyClass(mc: MyClass): JawaClass = {
    val typ = mc.typ
    val accessFlag = mc.accessFlag
    val clazz: JawaClass = JawaClass(this, typ, accessFlag)
    mc.fields foreach{
      f =>
        val fname = f.FQN.fieldName
        val ftyp = f.FQN.typ
        val faccessFlag = f.accessFlag
        JawaField(clazz, fname, ftyp, faccessFlag)
    }
    mc.methods foreach {
      m =>
        resolveFromMyMethod(clazz, m)
    }
    mc.superType.foreach(addNeedToResolveExtend(clazz, _))
    mc.outerType.foreach(addNeedToResolveOuterClass(clazz, _))
    mc.interfaces.foreach(addNeedToResolveExtend(clazz, _))
    clazz
  }
  
  protected def resolveFromMyMethod(clazz: JawaClass, m: MyMethod): JawaMethod = {
    val sig = m.signature
    val mname = sig.methodNamePart
    var paramNames = m.params
//    println(sig)
//    println(AccessFlag.toString(m.accessFlag))
//    println(paramNames)
    val thisOpt: Option[String] = (AccessFlag.isStatic(m.accessFlag) || AccessFlag.isNative(m.accessFlag) || AccessFlag.isAbstract(m.accessFlag)) match {
      case true => None
      case false => 
        val t = paramNames.head
        paramNames = paramNames.tail
        Some(t)
    }
    val paramsize = paramNames.size
    val params: MList[(String, JawaType)] = mlistEmpty
    val paramtyps = sig.getParameterTypes()
    for(i <- 0 to paramsize - 1){
      val pname = paramNames(i)
      val paramtyp = paramtyps(i)
      params += ((pname, paramtyp))
    }
    val retTyp = sig.getReturnType()
    val accessFlag = m.accessFlag
    
    JawaMethod(clazz, mname, thisOpt, params.toList, retTyp, accessFlag)
  }
}

object JawaResolver{
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
}