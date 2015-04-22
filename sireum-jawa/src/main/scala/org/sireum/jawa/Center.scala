/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa

import org.sireum.jawa.util.StringFormConverter
import org.sireum.util._
import org.sireum.jawa.MessageCenter._
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import org.sireum.jawa.xml.AndroidXStream
import java.io.File

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object Center {
  
  val DEBUG = false
  
  /**
   * set of classes contained by the current Center
   */
  
	private var classes : Set[JawaClass] = Set()
	
	/**
   * set of application classes contained by the current Center
   */
	
	private var applicationClasses : Set[JawaClass] = Set()
	
	/**
   * set of framework classes contained by the current Center
   */
	
	private var frameworkClasses : Set[JawaClass] = Set()
  
  /**
   * set of third party lib classes contained by the current Center
   */
  
  private var thirdPartyLibClasses : Set[JawaClass] = Set()
	
	/**
	 * map from clazz name to JawaClass
	 */
	
	private var nameToClass : Map[String, JawaClass] = Map()
	
	/**
   * main classes of the current Center
   */
	
	private var mainClass : JawaClass = null
	
	/**
   * set of entry points of the current Center
   */
	
	private var entryPoints : Set[JawaMethod] = Set()
	
	/**
	 * clazz hierarchy of all classes in the current Center
	 */
	
	private var hierarchy : ClassHierarchy = null
	
	val DEFAULT_TOPLEVEL_OBJECT = "java.lang.Object"
	  
	/**
	 * We need center clazz and procedure to provide the container for Context(Center, L0000) e.g. X.class
	 */
	final val CENTER_RECORD = "Center"
	final val CENTER_PROCEDURE_SIG = "LCenter;.center:()V"
	  
	/**
	 * We need Unknown clazz and procedure to account for Modeled calls and Native calls
	 */
//	final val UNKNOWN_RECORD = "Center.Unknown"
//	final val UNKNOWN_PROCEDURE_SIG = "LCenter/Unknown;.unknown:()LCenter/Unknown;"
	  
	val JAVA_PRIMITIVE_TYPES = Set("byte", "short", "int", "long", "float", "double", "boolean", "char")
	
	/**
   * before starting the analysis, prepares the Center with some additional info
   * a clazz named "Unknown" with a procedure called "unknown()" is added to the Center
   * this special clazz is used to handle out-of-scope calls 
   */
  def setupCenter = {
//    val unknown = new JawaClass
//    unknown.init(Center.UNKNOWN_RECORD)
//    unknown.setLibraryClass
//    val up = new JawaMethod
//    up.init(Center.UNKNOWN_PROCEDURE_SIG)
//    up.setPhantom
//    unknown.addMethod(up)
//    Center.addClass(unknown)
    
    val center = new JawaClass
    center.init(Center.CENTER_RECORD)
    center.setFrameworkClass
    center.setUnknown
    val cp = new JawaMethod
    cp.init(Center.CENTER_PROCEDURE_SIG)
    cp.setUnknown
    center.addMethod(cp)
    Center.addClass(center)
  }
  
  setupCenter
	
	/**
	 * map from global variable signature to uri; it's just a temp map
	 */
	
	private var globalVarSigToUri : Map[String, ResourceUri] = Map()
	
	def setGlobalVarSigToUri(sig : String, uri : ResourceUri) = {
    this.globalVarSigToUri += (sig -> uri)
  }
  
  def getGlobalVarUri(sig : String) = {
    this.globalVarSigToUri.get(sig)
  }
  
  /**
   * return whether given type is java primitive type
   */
  
  def isJavaPrimitiveType(typ : Type) : Boolean = !typ.isArray && this.JAVA_PRIMITIVE_TYPES.contains(typ.typ)
  
  /**
   * return whether given type is java primitive type
   */
  
  def isJavaPrimitiveType(name : String) : Boolean = this.JAVA_PRIMITIVE_TYPES.contains(name)
	  
	
	  
  /**
   * resolve classes relation
   */
  
  def resolveClassesRelation = {
    getClasses.foreach{
      clazz =>
        clazz.needToResolveOuterName match{
	        case Some(o) =>
	          tryGetClass(o) match{
		          case Some(outer) =>
		            clazz.needToResolveOuterName = None
		            clazz.setOuterClass(outer)
		          case None =>
		        }
	        case None =>
	      }
		    var resolved : Set[String] = Set()
        clazz.needToResolveExtends.foreach{
		      recName =>
		        tryGetClass(recName) match{
		          case Some(parent) =>
		            resolved += recName
		            if(parent.isInterface) clazz.addInterface(parent)
		            else clazz.setSuperClass(parent)
		          case None =>
		        }
		    }
		    clazz.needToResolveExtends --= resolved
    }
  }
  
  /**
   * resolve classes relation of the whole program
   */
  
  def resolveClassesRelationWholeProgram = {
//    if(GlobalConfig.mode < Mode.WHOLE_PROGRAM_TEST) throw new RuntimeException("It is not a whole program mode.")
    val worklist : MList[JawaClass] = mlistEmpty
    var codes : Set[String] = Set()
    worklist ++= getClasses
    do{
      codes = Set()
      var tmpList : List[JawaClass] = List()
	    while(!worklist.isEmpty){
	      val clazz = worklist.remove(0)
	      clazz.needToResolveOuterName match{
	        case Some(o) =>
	          tryGetClass(o) match{
		          case Some(outer) =>
		            clazz.needToResolveOuterName = None
		            clazz.setOuterClass(outer)
		            if(!outer.needToResolveExtends.isEmpty || outer.needToResolveOuterName.isDefined) worklist += outer
		          case None =>
		            if(JawaCodeSource.containsClass(o)){
			            val code = JawaCodeSource.getClassCode(o, ResolveLevel.HIERARCHY)
			            codes += code
			            tmpList ::= clazz
		            } else {
		              val unknownOut = resolveClass(o, ResolveLevel.HIERARCHY)
		              clazz.setOuterClass(unknownOut)
		            }
		        }
	        case None =>
	      }
	      var resolved : Set[String] = Set()
        clazz.needToResolveExtends.foreach{
	        parName =>
		        tryGetClass(parName) match{
		          case Some(parent) =>
		            resolved += parName
		            if(parent.isInterface) clazz.addInterface(parent)
		            else clazz.setSuperClass(parent)
		            if(!parent.needToResolveExtends.isEmpty || parent.needToResolveOuterName.isDefined) worklist += parent
		          case None =>
		            if(JawaCodeSource.containsClass(parName)){
			            val code = JawaCodeSource.getClassCode(parName, ResolveLevel.HIERARCHY)
			            codes += code
			            tmpList ::= clazz
		            } else {
		              val unknownSu = resolveClass(parName, ResolveLevel.HIERARCHY)
                  clazz.setSuperClass(unknownSu)
		            }
		        }
	      }
	      clazz.needToResolveExtends --= resolved
	    }
      worklist ++= tmpList
      if(!codes.isEmpty){
      	val st = Transform.getSymbolResolveResult(codes)
      	JawaResolver.resolveFromST(st, ResolveLevel.HIERARCHY, GlobalConfig.jawaResolverParallel)
      }
    }while(!codes.isEmpty)
      
    getClasses.foreach{
      rec =>
        if(!rec.isUnknown && !rec.hasSuperClass && rec.getName != DEFAULT_TOPLEVEL_OBJECT){
          if(!hasClass(DEFAULT_TOPLEVEL_OBJECT)) resolveClass(DEFAULT_TOPLEVEL_OBJECT, ResolveLevel.HIERARCHY)
          rec.setSuperClass(getClass(DEFAULT_TOPLEVEL_OBJECT))
        }
    }
  }
	
	/**
	 * get all the application classes
	 */
	
	def getApplicationClasses = this.applicationClasses
	
	/**
	 * get all the framework classes
	 */
	
	def getFrameworkClasses = this.frameworkClasses
  
  /**
   * get all the third party lib classes
   */
  
  def getThirdPartyLibClasses = this.thirdPartyLibClasses
	
	/**
	 * add an application clazz
	 */
	
	def addApplicationClass(ar : JawaClass) = {
    if(this.applicationClasses.contains(ar)) throw new RuntimeException("clazz " + ar.getName + " already exists in application clazz set.")
    this.applicationClasses += ar
  }
	
	/**
	 * add a framework clazz
	 */
	
	def addFrameworkClass(l : JawaClass) = {
    if(this.frameworkClasses.contains(l)) throw new RuntimeException("clazz " + l.getName + " already exists in framework clazz set.")
    else this.frameworkClasses += l
	}
  
  /**
   * add a framework clazz
   */
  
  def addThirdPartyLibClass(l : JawaClass) = {
    if(this.thirdPartyLibClasses.contains(l)) throw new RuntimeException("clazz " + l.getName + " already exists in third party lib clazz set.")
    else this.thirdPartyLibClasses += l
  }
	
	/**
	 * get classes
	 */
	
	def getClasses = this.classes
	
	/**
	 * return true if the center has given clazz
	 */
	
	def hasClass(name : String) : Boolean = this.nameToClass.contains(name)
	
	/**
	 * get clazz by a clazz name. e.g. java.lang.Object
	 */
	
	def getClass(name : String) : JawaClass =
	  this.nameToClass.getOrElse(name, throw new RuntimeException("clazz " + name + " does not exist in clazz set."))
	
	/**
	 * try to get clazz by name; if it does not exist, return None
	 */
	
	def tryGetClass(name : String) : Option[JawaClass] = {
	  this.nameToClass.get(name)
	}
	
	/**
	 * remove application clazz
	 */
	
	def removeApplicationClasses(ar : JawaClass) = {
    if(!this.applicationClasses.contains(ar)) throw new RuntimeException("clazz " + ar.getName + " does not exist in application clazz set.")
    else this.applicationClasses -= ar
  }
	
	/**
	 * remove framework clazz
	 */
	
	def removeFrameworkClasses(l : JawaClass) = {
    if(!this.frameworkClasses.contains(l)) throw new RuntimeException("clazz " + l.getName + " does not exist in framework clazz set.")
    else this.frameworkClasses -= l
	}
  
  /**
   * remove third party lib clazz
   */
  
  def removeThirdPartyLibClasses(l : JawaClass) = {
    if(!this.thirdPartyLibClasses.contains(l)) throw new RuntimeException("clazz " + l.getName + " does not exist in third party lib clazz set.")
    else this.thirdPartyLibClasses -= l
  }
	
	/**
	 * get containing set of given clazz
	 */
	
	def getContainingSet(ar : JawaClass) : Set[JawaClass] = {
    if(ar.isApplicationClass) this.applicationClasses
    else if(ar.isFrameworkClass) this.frameworkClasses
    else if(ar.isThirdPartyLibClass) this.thirdPartyLibClasses
    else null
  }
	
	/**
	 * remove given clazz from containing set
	 */
	
	def removeFromContainingSet(ar : JawaClass) = {
    if(ar.isApplicationClass) removeApplicationClasses(ar)
    else if(ar.isFrameworkClass) removeFrameworkClasses(ar)
    else if(ar.isThirdPartyLibClass) removeThirdPartyLibClasses(ar)
  }
	
	/**
	 * set main clazz
	 */
	
	def setMainClass(mr : JawaClass) = {
	  if(!mr.declaresMethod("main([Ljava/lang/String;)V")) throw new RuntimeException("Main clazz does not have Main procedure")
	  this.mainClass = mr
	}
	
	/**
	 * return has main clazz or not
	 */
	
	def hasMainClass : Boolean = this.mainClass != null
	
	/**
	 * get main clazz
	 */
	
	def getMainClass : JawaClass = {
	  if(!hasMainClass) throw new RuntimeException("No main clazz has been set!")
	  this.mainClass
	}
	
	/**
	 * get main clazz
	 */
	
	def tryGetMainClass : Option[JawaClass] = {
	  if(!hasMainClass) None
	  else Some(this.mainClass)
	}
	
	/**
	 * get main procedure
	 */
	
	def getMainMethod : JawaMethod = {
	  if(!hasMainClass) throw new RuntimeException("No main clazz has been set!")
	  if(!this.mainClass.declaresMethod("main([Ljava/lang/String;)V")) throw new RuntimeException("Main clazz does not have Main procedure")
	  this.mainClass.getMethod("main([Ljava/lang/String;)V")
	}
	
	/**
	 * because of some classes' changes we need to modify the hierarchy
	 */
	
	def modifyHierarchy = {
	  releaseClassHierarchy
	  
	}
	
	/**
	 * retrieve the normal clazz hierarchy
	 */
	
	def getClassHierarchy : ClassHierarchy ={
	  if(!hasClassHierarchy) setClassHierarchy(new ClassHierarchy().build)
	  this.hierarchy
	}
	
	/**
	 * set normal clazz hierarchy
	 */
	
	def setClassHierarchy(h : ClassHierarchy) = this.hierarchy = h
	
	/**
	 * check whether clazz hierarchy available or not
	 */
	
	def hasClassHierarchy : Boolean = this.hierarchy != null
	
	/**
	 * release clazz hierarchy
	 */
	
	def releaseClassHierarchy = this.hierarchy = null
	
	/**
	 * add clazz into Center
	 */
	
	def addClass(ar : JawaClass) = {
    if(ar.isInCenter) throw new RuntimeException("already in center: " + ar.getName)
    if(containsClass(ar.getName) && getClass(ar.getName).getResolvingLevel >= ar.getResolvingLevel) throw new RuntimeException("duplicate clazz: " + ar.getName)
	  tryRemoveClass(ar.getName)
    this.classes += ar
    if(ar.isArray){
      ar.setFrameworkClass
    } else if (JawaCodeSource.containsClass(ar.getName)){
	    JawaCodeSource.getCodeType(ar.getName) match{
	      case JawaCodeSource.CodeType.APP => ar.setApplicationClass
	      case JawaCodeSource.CodeType.THIRD_PARTY_LIB => ar.setThirdPartyLibClass
	      case JawaCodeSource.CodeType.FRAMEWORK => ar.setFrameworkClass
	    }
    } else {
      ar.setFrameworkClass
    }
    this.nameToClass += (ar.getName -> ar)
    ar.setInCenter(true)
    modifyHierarchy
  }
	
	/**
	 * remove clazz from Center
	 */
	
	def removeClass(ar : JawaClass) = {
	  if(!ar.isInCenter) throw new RuntimeException("does not exist in center: " + ar.getName)
	  this.classes -= ar
	  this.nameToClass -= ar.getName
	  if(ar.isFrameworkClass) this.frameworkClasses -= ar
    else if(ar.isThirdPartyLibClass) this.thirdPartyLibClasses -= ar
	  else if(ar.isApplicationClass) this.applicationClasses -= ar
	  ar.setInCenter(false)
	  modifyHierarchy
	}
	
	/**
	 * try to remove clazz from Center
	 */
	
	def tryRemoveClass(recordName : String) = {
	  val aropt = tryGetClass(recordName)
	  aropt match{
	    case Some(ar) =>
			  removeClass(ar)
	    case None =>
	  }
	}
	
	/**
	 * get clazz name from procedure name. e.g. java.lang.Object.equals -> java.lang.Object
	 */
	
	def procedureNameToClassName(name : String) : String = {
	  val index = name.lastIndexOf('.')
	  if(index < 0) throw new RuntimeException("wrong procedure name: " + name)
	  name.substring(0, index)
	}
	
	/**
	 * get clazz name from procedure signature. e.g. Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z -> java.lang.Object
	 */
	
	def getClassNameFromMethodSignature(sig : String) : String = StringFormConverter.getClassNameFromMethodSignature(sig)
	
	/**
	 * convert type string from signature style to type style. Ljava/lang/Object; -> java.lang.Object 
	 */
	
	def formatSigToTypeForm(sig : String) : Type = StringFormConverter.formatSigToTypeForm(sig)
	
	/**
	 * get sub-signature from signature. e.g. Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z -> equals:(Ljava/lang/Object;)Z
	 */
	
	def getSubSigFromMethodSig(sig : String) : String = StringFormConverter.getSubSigFromMethodSig(sig)
	
	/**
	 * get outer class name from inner class name
	 */
	
	def getOuterNameFrom(innerName : String) : String = StringFormConverter.getOuterNameFrom(innerName)
	
	/**
	 * return true if the given name is a inner class name or not
	 */
	
	def isInnerClassName(name : String) : Boolean = name.lastIndexOf("$") > 0
	
	/**
	 * current Center contains the given clazz or not
	 */
	
	def containsClass(ar : JawaClass) = ar.isInCenter
	
	/**
	 * current Center contains the given clazz or not
	 */
	
	def containsClass(name : String) = this.nameToClass.contains(name)
	
	/**
	 * grab field from Center. Input example is java.lang.Throwable.stackState
	 */
	def getField(fieldSig : String) : Option[JawaField] = {
	  val rName = StringFormConverter.getClassNameFromFieldSignature(fieldSig)
	  if(!containsClass(rName)) return None
	  val r = getClass(rName)
	  if(!r.declaresField(fieldSig)) return None
	  Some(r.getField(fieldSig))
	}
	
	/**
	 * return true if contains the given field. Input example is java.lang.Throwable.stackState
	 */
	
	def containsField(fieldSig : String) : Boolean = getField(fieldSig).isDefined
	
	/**
	 * get procedure from Center. Input example is Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z
	 */
	
	def getMethod(procSig : String) : Option[JawaMethod] = {
	  val rName = StringFormConverter.getClassNameFromMethodSignature(procSig)
	  val subSig = getSubSigFromMethodSig(procSig)
	  if(!containsClass(rName)) return None
	  val r = getClass(rName)
	  r.tryGetMethod(subSig)
	}
	
	def getMethodDeclarations(procSig : String) : Set[JawaMethod] = {
	  val result : MSet[JawaMethod] = msetEmpty
	  val rName = StringFormConverter.getClassNameFromMethodSignature(procSig)
	  val subSig = getSubSigFromMethodSig(procSig)
	  if(!containsClass(rName)) resolveClass(rName, ResolveLevel.HIERARCHY)
	  val r = getClass(rName)
	  val worklist : MList[JawaClass] = mlistEmpty
	  worklist += r
	  while(!worklist.isEmpty){
	    val rec = worklist.remove(0)
	    rec.tryGetMethod(subSig) match{
	      case Some(proc) => result += proc
	      case None =>
	        if(rec.hasSuperClass) worklist += rec.getSuperClass
	        worklist ++= rec.getInterfaces
	    }
	  }
	  result.toSet
	}
	
	/**
	 * return true if contains the given procedure. Input example is Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z
	 */
	
	def containsMethod(procSig : String) : Boolean = getMethod(procSig).isDefined
	
	/**
	 * get field from Center. Input example is java.lang.Throwable.stackState
	 */
	def getFieldWithoutFailing(fieldSig : String) : JawaField = {
	  getField(fieldSig) match{
	    case Some(f) => f
	    case None => throw new RuntimeException("Given field signature: " + fieldSig + " is not in the Center.")
	  }
	}
	
	/**
	 * find field from Center. Input: java.lang.Throwable.stackState
	 */
	def findField(baseType : Type, fieldSig : String) : Option[JawaField] = {
	  val rName = baseType.name
	  val fieldName = StringFormConverter.getFieldNameFromFieldSignature(fieldSig)
	  tryLoadClass(rName, ResolveLevel.HIERARCHY)
	  if(!containsClass(rName)) return None
	  var r = getClass(rName)
	  while(!r.declaresFieldByName(fieldName) && r.hasSuperClass){
	    r = r.getSuperClass
	  }
	  if(!r.declaresFieldByName(fieldName)) return None
	  Some(r.getFieldByName(fieldName))
	}
	
	/**
	 * find field from Center. Input: java.lang.Throwable.stackState
	 */
	def findFieldWithoutFailing(baseType : Type, fieldSig : String) : JawaField = {
	  findField(baseType, fieldSig).getOrElse(throw new RuntimeException("Given baseType " + baseType + " and field signature " + fieldSig + " is not in the Center."))
	}
	
	/**
	 * find field from Center. Input: @@java.lang.Throwable.stackState
	 */
	def findStaticField(fieldSig : String) : Option[JawaField] = {
	  val baseType = StringFormConverter.getClassTypeFromFieldSignature(fieldSig)
	  val rName = baseType.name
	  val fieldName = StringFormConverter.getFieldNameFromFieldSignature(fieldSig)
	  tryLoadClass(rName, ResolveLevel.HIERARCHY)
	  if(!containsClass(rName)) return None
	  var r = getClass(rName)
	  while(!r.declaresFieldByName(fieldName) && r.hasSuperClass){
	    r = r.getSuperClass
	  }
	  if(!r.declaresFieldByName(fieldName)) return None
	  val f = r.getFieldByName(fieldName)
	  if(f.isStatic)
	  	Some(f)
	  else None
	}
	
	/**
	 * find field from Center. Input: @@java.lang.Throwable.stackState
	 */
	def findStaticFieldWithoutFailing(fieldSig : String) : JawaField = {
	  findStaticField(fieldSig).getOrElse(throw new RuntimeException("Given static field signature " + fieldSig + " is not in the Center."))
	}
	
	/**
	 * get procedure from Center. Input: Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z
	 */
	
	def getMethodWithoutFailing(procSig : String) : JawaMethod = {
	  getMethod(procSig) match{
	    case Some(p) => p
	    case None => throw new RuntimeException("Given procedure signature: " + procSig + " is not in the Center.")
	  }
	}
	
	/**
	 * get entry points
	 */
	
	def getEntryPoints = {
	  if(!hasEntryPoints) findEntryPoints("main")
	  this.entryPoints
	}
	
	/**
	 * get entry points
	 */
	
	def getEntryPoints(entryMethodName : String) = {
	  if(hasEntryPoints) this.entryPoints == Set()
	  findEntryPoints(entryMethodName)
	  this.entryPoints
	}
	  
	/**
	 * set entry points
	 */
	
	def setEntryPoints(entryPoints : Set[JawaMethod]) = this.entryPoints ++= entryPoints
	
	/**
	 * find entry points from current app/test cases
	 */
	
	def findEntryPoints(entryMethodName : String) = {
	  getApplicationClasses.foreach{
	    appRec =>
	      if(appRec.declaresMethodByShortName(entryMethodName))
	        this.entryPoints += appRec.getMethodByShortName(entryMethodName)
	  }
	}
	
	/**
	 * has entry points
	 */
	
	def hasEntryPoints : Boolean = !this.entryPoints.isEmpty
	
	/**
	 * enum of all the valid resolve level of clazz
	 */
	
	object ResolveLevel extends Enumeration {
	  val HIERARCHY, BODY = Value
	}
	
	/**
	 * try to resolve given clazz and load all of the required support based on your desired resolve level.
	 */
	
	def tryLoadClass(recordName : String, desiredLevel : ResolveLevel.Value) : Option[JawaClass] = {
	  this.synchronized{
	  	JawaResolver.tryResolveClass(recordName, desiredLevel)
	  }
	}
	
	/**
	 * resolve given clazz and load all of the required support.
	 */
	
	def loadClassAndSupport(recordName : String) : JawaClass = {
	  this.synchronized{
	  	JawaResolver.resolveClass(recordName, ResolveLevel.BODY)
	  }
	}
	
	/**
	 * resolve given clazz.
	 */
	
	def resolveClass(recordName : String, desiredLevel : ResolveLevel.Value) : JawaClass = {
	  this.synchronized{
	  	JawaResolver.resolveClass(recordName, desiredLevel)
	  }
	}
	
	/**
	 * softly resolve given clazz.
	 */
	
	def softlyResolveClass(recordName : String, desiredLevel : ResolveLevel.Value) : Option[JawaClass] = {
	  this.synchronized{
		  if(JawaCodeSource.containsClass(recordName))
		  	Some(JawaResolver.resolveClass(recordName, desiredLevel))
		  else None
	  }
	}
	
	/**
	 * force resolve given clazz to given level
	 */
	
	def forceResolveClass(recordName : String, desiredLevel : ResolveLevel.Value) : JawaClass = {
	  this.synchronized{
	  	JawaResolver.forceResolveClass(recordName, desiredLevel)
	  }
	}
	
	/**
	 * init center with a image file
	 */
	
	def init(file : File) = {
	  val reader = new GZIPInputStream(new FileInputStream(file))
    val img = AndroidXStream.fromXml(reader).asInstanceOf[CenterImage]
	  restore(img)
	}
	
	/**
	 * reset the current center
	 */
	
	def reset = {
	  this.classes = Set()
	  this.applicationClasses = Set()
	  this.frameworkClasses = Set()
    this.thirdPartyLibClasses = Set()
	  this.nameToClass = Map()
	  this.mainClass = null
	  this.entryPoints = Set()
	  this.hierarchy = null
	  setupCenter
	}
	
	/**
	 * Create a class to store all center informations
	 */
	
	class CenterImage {
  	var classes : Set[JawaClass] = Center.classes
  	var applicationClasses : Set[JawaClass] = Center.applicationClasses
  	var frameworkClasses : Set[JawaClass] = Center.frameworkClasses
    var thirdPartyLibClasses : Set[JawaClass] = Center.thirdPartyLibClasses
  	var nameToClass : Map[String, JawaClass] = Center.nameToClass
  	var mainClass : JawaClass = Center.mainClass
  	var entryPoints : Set[JawaMethod] = Center.entryPoints
  	var hierarchy : ClassHierarchy = Center.hierarchy
  	
//  	override def equals(obj : Any) : Boolean = {
//  	  obj match {
//  	    case img : CenterImage =>
//  	      if(classes == img.classes &&
//  	         applicationClasses == img.applicationClasses &&
//  	         libraryClasses == img.libraryClasses &&
//  	         nameToClass == img.nameToClass &&
//  	         entryPoints == img.entryPoints 
////  	         hierarchy == img.hierarchy
//  	         )
//  	        true
//  	      else false
//  	    case _ => false
//  	  }
//  	}
	}
	
	/**
	 * Create a image of current Center
	 */
	
	def createImage : CenterImage = {
	  new CenterImage
	}
	
	/**
	 * restore center from a image
	 */
	
	def restore(img : CenterImage) = {
	  reset
	  this.classes = img.classes
	  this.applicationClasses = img.applicationClasses
	  this.frameworkClasses = img.frameworkClasses
    this.thirdPartyLibClasses = img.thirdPartyLibClasses
	  this.nameToClass = img.nameToClass
	  this.mainClass = img.mainClass
	  this.entryPoints = img.entryPoints
	  this.hierarchy = img.hierarchy
	}
	
	def printDetails = {
	  println("***************Center***************")
	  println("applicationClasses: " + getApplicationClasses)
    println("thirdPartyLibClasses: " + getThirdPartyLibClasses)
	  println("frameworkClasses: " + getFrameworkClasses)
	  println("noCategorizedClasses: " + (getClasses -- getFrameworkClasses -- getThirdPartyLibClasses -- getApplicationClasses))
	  println("mainClass: " + tryGetMainClass)
	  println("entryPoints: " + getEntryPoints)
	  println("hierarchy: " + getClassHierarchy)
	  if(DEBUG){
	  	getClasses.foreach{
	  	  case r=>
	  	  	r.printDetail
	  	  	r.getFields.foreach(_.printDetail)
	  	  	r.getMethods.foreach(_.printDetail)
	  	}
	  	getClassHierarchy.printDetails
	  }
	  println("******************************")
	}
	
}