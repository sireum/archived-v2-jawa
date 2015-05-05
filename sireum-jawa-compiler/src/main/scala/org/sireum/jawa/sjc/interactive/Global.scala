/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.interactive

import org.sireum.util._
import org.sireum.jawa.sjc.ResolveLevel
import org.sireum.jawa.sjc.JawaType
import org.sireum.jawa.sjc.Signature
import org.sireum.jawa.sjc.ObjectType

/**
 * This is the interactive compiler of Jawa.
 * 
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class Global(projectName: String) extends RichCompilationUnits with JawaClassLoadManager with ResolveLevel {
  
  /**
   * set of classes contained by the current Global
   */
	private val classes: MMap[ObjectType, JawaClass] = mmapEmpty
	
	/**
   * set of application classes contained by the current Global
   */
	private val applicationClasses: MMap[ObjectType, JawaClass] = mmapEmpty
	
	/**
   * set of system library classes contained by the current Global
   */
	private val systemLibraryClasses: MMap[ObjectType, JawaClass] = mmapEmpty
  
  /**
   * set of third party lib classes contained by the current Global
   */
  private val thirdPartyLibraryClasses: MMap[ObjectType, JawaClass] = mmapEmpty
	
	/**
   * set of entry points of the current Global
   */
	private val entryPoints: MSet[JawaMethod] = msetEmpty
	
	/**
	 * class hierarchy of all classes in the current Global
	 */
	private val hierarchy: ClassHierarchy = new ClassHierarchy
	  
  
	
	/**
	 * get all the application classes
	 */
	def getApplicationClasses: ISet[JawaClass] = this.applicationClasses.values.toSet
	
	/**
	 * get all the system library classes
	 */
	def getSystemLibraryClasses: ISet[JawaClass] = this.systemLibraryClasses.values.toSet
  
  /**
   * get all the third party lib classes
   */
  def getThirdPartyLibraryClasses: ISet[JawaClass] = this.thirdPartyLibraryClasses.values.toSet
  
  /**
   * get all the application classes
   */
  def isApplicationClasses(typ: ObjectType): Boolean = this.applicationClasses.contains(typ)
  
  /**
   * get all the system library classes
   */
  def isSystemLibraryClasses(typ: ObjectType): Boolean = this.systemLibraryClasses.contains(typ)
  
  /**
   * get all the third party lib classes
   */
  def isThirdPartyLibraryClasses(typ: ObjectType): Boolean = this.thirdPartyLibraryClasses.contains(typ)
	
	/**
	 * add an application class
	 */
	def addApplicationClass(ar: JawaClass) = {
    if(this.applicationClasses.contains(ar.getType)) throw new RuntimeException("class " + ar.getName + " already exists in application class set.")
    this.applicationClasses(ar.getType) = ar
  }
	
	/**
	 * add a system library class
	 */
	def addSystemLibraryClass(l: JawaClass) = {
    if(this.systemLibraryClasses.contains(l.getType)) throw new RuntimeException("class " + l.getName + " already exists in system library class set.")
    else this.systemLibraryClasses(l.getType) = l
	}
  
  /**
   * add a third party library class
   */
  def addThirdPartyLibraryClass(l: JawaClass) = {
    if(this.thirdPartyLibraryClasses.contains(l.getType)) throw new RuntimeException("class " + l.getName + " already exists in third party lib class set.")
    else this.thirdPartyLibraryClasses(l.getType) = l
  }
  
  /**
   * has class
   */
  def hasClass(typ: ObjectType) = this.classes.contains(typ)
	
	/**
	 * get classes
	 */
	def getClasses: ISet[JawaClass] = this.classes.values.toSet
	
	/**
	 * get class by a class name. e.g. java.lang.Object
	 */
	def getClass(typ: ObjectType): JawaClass =
	  this.classes.getOrElse(typ, throw new RuntimeException("class " + typ + " does not exist in class set."))
	
	/**
	 * try to get class by name; if it does not exist, return None
	 */
	def tryGetClass(typ: ObjectType): Option[JawaClass] = {
	  this.classes.get(typ)
	}
	
	/**
	 * remove application class
	 */
	def removeApplicationClass(ar: JawaClass) = {
    if(!this.applicationClasses.contains(ar.getType)) throw new RuntimeException("class " + ar.getName + " does not exist in application class set.")
    else this.applicationClasses -= ar.getType
  }
	
	/**
	 * remove System Library Class
	 */
	def removeSystemLibraryClass(l: JawaClass) = {
    if(!this.systemLibraryClasses.contains(l.getType)) throw new RuntimeException("class " + l.getType + " does not exist in framework class set.")
    else this.systemLibraryClasses -= l.getType
	}
  
  /**
   * remove third party lib class
   */
  def removeThirdPartyLibraryClass(l: JawaClass) = {
    if(!this.thirdPartyLibraryClasses.contains(l.getType)) throw new RuntimeException("class " + l.getType + " does not exist in third party lib class set.")
    else this.thirdPartyLibraryClasses(l.getType) = l
  }
	
	/**
	 * get containing set of given class
	 */
	def getContainingSet(ar: JawaClass): Set[JawaClass] = {
    if(ar.isApplicationClass) getApplicationClasses
    else if(ar.isSystemLibraryClass) getSystemLibraryClasses
    else if(ar.isThirdPartyLibraryClass) getThirdPartyLibraryClasses
    else null
  }
	
	/**
	 * remove given class from containing set
	 */
	def removeFromContainingSet(ar: JawaClass) = {
    if(ar.isApplicationClass) removeApplicationClass(ar)
    else if(ar.isSystemLibraryClass) removeSystemLibraryClass(ar)
    else if(ar.isThirdPartyLibraryClass) removeThirdPartyLibraryClass(ar)
  }
	
	/**
	 * retrieve the normal class hierarchy
	 */
	def getClassHierarchy: ClassHierarchy ={
	  this.hierarchy.build(this)
	  this.hierarchy
	}
	
	/**
	 * reset class hierarchy
	 */
	def resetClassHierarchy = this.hierarchy.reset
	
	/**
	 * add class into Global
	 */
	def addClass(ar: JawaClass) = {
    if(containsClass(ar) && getClass(ar.getType).getResolvingLevel >= ar.getResolvingLevel) throw new RuntimeException("duplicate class: " + ar.getName)
	  tryRemoveClass(ar.getType)
    this.classes(ar.getType) = ar
    if(ar.isArray){
      ar.setSystemLibraryClass
    } else if (JawaCodeSource.containsClass(ar.getType)){
	    JawaCodeSource.getCodeType(ar.getType) match{
	      case JawaCodeSource.CodeType.APP => ar.setApplicationClass
	      case JawaCodeSource.CodeType.THIRD_PARTY_LIBRARY => ar.setThirdPartyLibraryClass
	      case JawaCodeSource.CodeType.SYSTEM_LIBRARY => ar.setSystemLibraryClass
	    }
    } else {
      ar.setSystemLibraryClass
    }
    modifyHierarchy
  }
	
	/**
	 * remove class from Global
	 */
	
	def removeClass(ar: JawaClass) = {
	  if(!containsClass(ar)) throw new RuntimeException("does not exist in Global: " + ar.getName)
	  this.classes -= ar.getType
	  if(ar.isSystemLibraryClass) this.systemLibraryClasses -= ar.getType
    else if(ar.isThirdPartyLibraryClass) this.thirdPartyLibraryClasses -= ar.getType
	  else if(ar.isApplicationClass) this.applicationClasses -= ar.getType
	  modifyHierarchy
	}
  
  def modifyHierarchy = {
    if(isDirty) resolveClassesRelationWholeProgram
    this.hierarchy.build(this)
  }
	
	/**
	 * try to remove class from Global
	 */
	def tryRemoveClass(typ: ObjectType) = {
	  val aropt = tryGetClass(typ)
	  aropt match{
	    case Some(ar) =>
			  removeClass(ar)
	    case None =>
	  }
	}
	
	/**
	 * current Global contains the given class or not
	 */
	def containsClass(ar: JawaClass) = ar.global == this
	
	/**
	 * current Global contains the given class or not
	 */
	def containsClass(typ: ObjectType) = this.classes.contains(typ)
	
	/**
	 * grab field from Global. Input example is java.lang.Throwable.stackState
	 */
	def getField(fieldFQN: String): Option[JawaField] = {
    try{
  	  val rName = getClassNameFromFieldFQN(fieldFQN)
      val rType: ObjectType = getTypeFromName(rName).asInstanceOf[ObjectType]
      val fName = getFieldNameFromFieldFQN(fieldFQN)
  	  if(!containsClass(rType)) return None
  	  val r = getClass(rType)
  	  if(!r.hasField(fName)) return None
  	  Some(r.getField(fName))
    } catch {
      case t: Throwable => None
    }
	}
	
	/**
	 * return true if contains the given field. Input example is java.lang.Throwable.stackState
	 */
	
	def containsField(fieldFQN: String): Boolean = getField(fieldFQN).isDefined
	
	/**
	 * get procedure from Global. Input example is Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z
	 */
	
	def getMethod(signature: Signature): Option[JawaMethod] = {
	  val rType = signature.getClassType
	  val subSig = signature.getSubSignature
	  if(!containsClass(rType)) return None
	  val r = getClass(rType)
	  r.tryGetMethod(subSig)
	}
	
	def getMethodDeclarations(signature: Signature): Set[JawaMethod] = {
	  val result: MSet[JawaMethod] = msetEmpty
	  val rType = signature.getClassType
    val subSig = signature.getSubSignature
	  if(!containsClass(rType)) resolveClass(rType, ResolveLevel.HIERARCHY)
	  val r = getClass(rType)
	  val worklist: MList[JawaClass] = mlistEmpty
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
	def containsMethod(signature: Signature): Boolean = getMethod(signature).isDefined
	
	/**
	 * get field from Global. Input example is java.lang.Throwable.stackState
	 */
	def getFieldWithoutFailing(fieldSig: String): JawaField = {
	  getField(fieldSig) match{
	    case Some(f) => f
	    case None => throw new RuntimeException("Given field signature: " + fieldSig + " is not in the Global.")
	  }
	}
	
	/**
	 * find field from Global. Input: stackState
	 */
	def findField(baseType: ObjectType, fieldName: String): Option[JawaField] = {
	  tryLoadClass(baseType, ResolveLevel.HIERARCHY)
	  if(!containsClass(baseType)) return None
	  val r = getClass(baseType)
    r.tryGetField(fieldName)
	}
	
	/**
	 * find field from Global. Input: java.lang.Throwable.stackState
	 */
	def findFieldWithoutFailing(baseType: ObjectType, fieldSig: String): JawaField = {
	  findField(baseType, fieldSig).getOrElse(throw new RuntimeException("Given baseType " + baseType + " and field signature " + fieldSig + " is not in the Global."))
	}
	
	/**
	 * find field from Global. Input: @@java.lang.Throwable.stackState
	 */
	def findStaticField(fieldFQN: String): Option[JawaField] = {
	  val rName = getClassNameFromFieldFQN(fieldFQN)
    val rType = getTypeFromName(rName).asInstanceOf[ObjectType]
	  val fieldName = getFieldNameFromFieldFQN(fieldFQN)
	  val classOpt = tryLoadClass(rType, ResolveLevel.HIERARCHY)
	  if(!classOpt.isDefined) return None
	  val r = classOpt.get
	  r.tryGetField(fieldName) match {
      case Some(f) =>  
        if(f.isStatic)
          Some(f)
        else None
      case None => None
    }
	  
	}
	
	/**
	 * find field from Global. Input: @@java.lang.Throwable.stackState
	 */
	def findStaticFieldWithoutFailing(fieldSig: String): JawaField = {
	  findStaticField(fieldSig).getOrElse(throw new RuntimeException("Given static field signature " + fieldSig + " is not in the Global."))
	}
	
	/**
	 * get procedure from Global. Input: Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z
	 */
	
	def getMethodWithoutFailing(signature: Signature): JawaMethod = {
	  getMethod(signature) match{
	    case Some(p) => p
	    case None => throw new RuntimeException("Given procedure signature: " + signature + " is not in the Global.")
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
	
	def getEntryPoints(entryMethodName: String) = {
	  if(hasEntryPoints) this.entryPoints == Set()
	  findEntryPoints(entryMethodName)
	  this.entryPoints
	}
	  
	/**
	 * set entry points
	 */
	
	def setEntryPoints(entryPoints: Set[JawaMethod]) = this.entryPoints ++= entryPoints
	
	/**
	 * find entry points from current app/test cases
	 */
	
	def findEntryPoints(entryMethodName: String) = {
	  getApplicationClasses.foreach{
	    appRec =>
	      if(appRec.declaresMethodByName(entryMethodName))
	        this.entryPoints += appRec.getMethodByName(entryMethodName)
	  }
	}
	
	/**
	 * has entry points
	 */
	def hasEntryPoints: Boolean = !this.entryPoints.isEmpty
	
	/**
	 * try to resolve given class and load all of the required support based on your desired resolve level.
	 */
	def tryLoadClass(typ: ObjectType, desiredLevel: ResolveLevel.Value): Option[JawaClass] = {
	  this.synchronized{
	  	tryResolveClass(typ, desiredLevel)
	  }
	}
	
	/**
	 * resolve given class and load all of the required support.
	 */
	
	def loadClassAndSupport(typ: ObjectType): JawaClass = {
	  this.synchronized{
	  	resolveClass(typ, ResolveLevel.BODY)
	  }
	}
	
	/**
	 * resolve given class.
	 */
	
	override def resolveClass(typ: ObjectType, desiredLevel: ResolveLevel.Value): JawaClass = {
	  this.synchronized{
	  	super.resolveClass(typ, desiredLevel)
	  }
	}
	
	/**
	 * softly resolve given class.
	 */
	
	def softlyResolveClass(typ: ObjectType, desiredLevel: ResolveLevel.Value): Option[JawaClass] = {
	  this.synchronized{
		  if(JawaCodeSource.containsClass(typ))
		  	Some(resolveClass(typ, desiredLevel))
		  else None
	  }
	}
	
	/**
	 * force resolve given class to given level
	 */
	
	override def forceResolveClass(typ: ObjectType, desiredLevel: ResolveLevel.Value): JawaClass = {
	  this.synchronized{
	  	super.forceResolveClass(typ, desiredLevel)
	  }
	}
	
	/**
	 * reset the current Global
	 */
	
	def reset = {
	  this.classes.clear()
	  this.applicationClasses.clear()
	  this.systemLibraryClasses.clear()
    this.thirdPartyLibraryClasses.clear()
	  this.entryPoints.clear()
	  this.hierarchy.reset
	}
	
	def printDetails = {
	  println("***************Global***************")
	  println("applicationClasses: " + getApplicationClasses)
    println("thirdPartyLibraryClasses: " + getThirdPartyLibraryClasses)
	  println("systemLibraryClasses: " + getSystemLibraryClasses)
	  println("noCategorizedClasses: " + (getClasses -- getSystemLibraryClasses -- getThirdPartyLibraryClasses -- getApplicationClasses))
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