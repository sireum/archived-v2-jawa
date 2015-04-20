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


/**
 * This class is an jawa class representation of a pilar record. A record corresponds to a class or an interface of the source code. They are usually created by jawa Resolver.
 * You can also construct it manually. Call init() method first when you want to do any further things.
 * 
 * 
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 *
 */
class JawaClass extends ResolveLevel {
  
  val constructorName : String = "<init>"
	val staticInitializerName : String = "<clinit>"
  
  /**
   * name is with this style: java.lang.Object or java.lang.Object[]
   */
  
  protected var name : String = null
  
  /**
   * shortName is with this style: Object or Object[]
   */
  
  protected var shortName : String = null
  
  /**
   * packageName is with this style: java:lang
   */
  
  protected var packageName : String = null
  
  /**
   * the access flags integer representation for this record
   */
  
  protected var accessFlags : Int = 0
  
  /**
   * is this record in amandroid Center or not?
   */
  
  protected var inCenter : Boolean = false
  
  /**
   * set of fields which belong to this record
   */
  
  protected var fields : Set[JawaField] = Set()
  
  /**
   * set of procedures which belong to this record
   */
  
  protected var procedures : Set[JawaMethod] = Set()
  
  /**
   * set of interfaces which this record extends or implements 
   */
  
  protected var interfaces : Set[JawaClass] = Set()
  
  /**
   * super class of this record
   */
  
  protected var superClass : JawaClass = null
  
  /**
   * outer class of this record
   */
  
  protected var outerClass : JawaClass = null
  
  /**
   * map from sub-signature to procedure
   */
  
  protected var subSigToMethods : Map[String, JawaMethod] = Map()
  
  /**
   * true: is phantom, which means it's not available in our code repo
   */
  
  protected var unknown : Boolean = false
  
  /**
   * didn't resolve this extends-relation list. It's a set of record names.
   */
  
  var needToResolveExtends : Set[String] = Set()
  
  /**
   * didn't resolve this outer class name. 
   */
  
  var needToResolveOuterName : Option[String] = None
  
  
  
  /**
   * add need-to-resolve-extend-record
   */
  
  def addNeedToResolveExtend(recName : String) = this.needToResolveExtends += recName
  
  /**
   * add need-to-resolve-extend-records
   */
  
  def addNeedToResolveExtends(recNames : Set[String]) = this.needToResolveExtends ++= recNames
  
  def setUnknown = this.unknown = true
  
  /**
   * return true if this record is phantom record
   */
  
  def isUnknown = this.unknown
  
  /**
   * when you construct a amandroid record instance, call this init function first
   */
  
  def init(name : String, accessFlags : Int): JawaClass = {
    setName(name)
    this.accessFlags = accessFlags
    this
  }
  
  /**
   * when you construct a amandroid record instance, call this init function first
   */
  
  def init(name : String): JawaClass = init(name, 0)
  
  /**
   * if the amandroidrecord is array type return true
   */
  
  def isArray : Boolean = getName.contains("[]")
  
  /**
   * parse the given record name to get the short name and the package name.
   */
  
  def setName(name : String) = {
    this.name = name
    val index = name.lastIndexOf('.')
    if(index > 0){
      if(isArray){
        this.shortName = name.substring(index + 1)
        this.packageName = null
      } else {
	      this.shortName = name.substring(index + 1)
	      this.packageName = name.substring(0, index)
      }
    } else {
      if(isArray){
        this.shortName = name
        this.packageName = null
      } else {
	      this.shortName = name
	      this.packageName = ""
      }
    }
  }
  
  /**
   * get name of the record
   */
  
  def getName = this.name
  
  /**
   * get short name of the record
   */
  
  def getShortName = this.shortName
  
  /**
   * get package name of the record
   */
  
  def getPackageName = this.packageName
	
	/**
	 * return whether this record in the Center or not.
	 */
	
	def isInCenter = inCenter
	
	/**
	 * sets the record inCenter status
	 */
	
	def setInCenter(ic : Boolean) = this.inCenter = ic
	
	/**
	 * return the access flags for this record
	 */
	
	def getAccessFlags = accessFlags
	
	/**
	 * return the access flags for this record
	 */
	
	def getAccessFlagString = AccessFlag.toString(getAccessFlags)
	
	/**
	 * sets the access flags for this record
	 */
	
	def setAccessFlags(af : Int) = this.accessFlags = af
	
	/**
	 * sets the access flags for this record
	 */
	
	def setAccessFlags(str : String) = this.accessFlags = AccessFlag.getAccessFlags(str)
	
	/**
	 * return the number of fields declared in this record
	 */
	
	def fieldSize = this.fields.size
	
	/**
	 * get all the fields accessible from the record
	 */
	
	def getFields = {
    var results = getDeclaredFields
    var worklist : Set[JawaClass] = Set()
    worklist += this
    while(!worklist.isEmpty){
      worklist =
	      worklist.map{
	        rec =>
	          var parents = rec.getInterfaces
	          if(rec.hasSuperClass) parents += rec.getSuperClass
	          val fields =
		          if(!parents.isEmpty)
			          parents.map{
			          	parent =>
			          	  parent.getDeclaredFields.filter(f => !f.isPrivate && !results.exists(_.getName == f.getName))
			          }.reduce(iunion[JawaField])
			        else Set[JawaField]()
	          results ++= fields
	          parents
	      }.reduce(iunion[JawaClass])
    }
    results
  }
	
	/**
	 * get all the fields declared in this record
	 */
	
	def getDeclaredFields = this.fields
	
	/**
	 * get all static fields of the record
	 */
	
	def getStaticFields = getFields.filter(f => f.isStatic)
	
	/**
	 * get all non-static fields of the record
	 */
	
	def getNonStaticFields = getFields.filter(f => !f.isStatic)
	
	/**
	 * get all object type field
	 */
	
	def getObjectTypeFields = getFields.filter(f => f.isObject)
	
	/**
	 * get all non static and object type field
	 */
	
	def getNonStaticObjectTypeFields = getNonStaticFields.intersect(getObjectTypeFields)
	
	/**
	 * get all static and object type field
	 */
	
	def getStaticObjectTypeFields = getStaticFields.intersect(getObjectTypeFields)
	
	/**
	 * get all static fields of the record
	 */
	
	def getDeclaredStaticFields = getDeclaredFields.filter(f => f.isStatic)
	
	/**
	 * get all non-static fields of the record
	 */
	
	def getDeclaredNonStaticFields = getDeclaredFields.filter(f => !f.isStatic)
	
	/**
	 * get all object type field
	 */
	
	def getDeclaredObjectTypeFields = getDeclaredFields.filter(f => f.isObject)
	
	/**
	 * get all non static and object type field
	 */
	
	def getDeclaredNonStaticObjectTypeFields = getDeclaredNonStaticFields.intersect(getDeclaredObjectTypeFields)
	
	/**
	 * get all static and object type field
	 */
	
	def getDeclaredStaticObjectTypeFields = getDeclaredStaticFields.intersect(getDeclaredObjectTypeFields)
	
	/**
	 * add one field into the record
	 */
	
	def addField(field : JawaField) = {
	  if(field.isDeclared) throw new RuntimeException("already declared: " + field.getName)
	  this.fields += field
	  field.setDeclaringClass(this)
	}
	
  /**
   * return true if the field is declared in this record
   */
  
	def declaresField(sig : String) : Boolean = !getDeclaredFields.filter(_.getSignature == sig).isEmpty
	
	/**
	 * whether this record declares a field with the given name
	 */
	
	def declaresFieldByName(name : String) = !getDeclaredFields.filter(_.getName == name).isEmpty
	
	/**
   * return true if the field is declared in this record
   */
  
	def hasField(sig : String) : Boolean = {
	  val name = StringFormConverter.getFieldNameFromFieldSignature(sig)
	  hasFieldByName(name)
	}
	
	/**
	 * whether this record declares a field with the given name
	 */
	
	def hasFieldByName(name : String) = !getFields.filter(_.getName == name).isEmpty
	
	/**
	 * whether this record declares a field with the given name and type
	 */
	
	def declaresField(name : String, typ : Type) = !getDeclaredFields.filter(f => (f.getName == name && f.getType == typ)).isEmpty
	
	/**
	 * removes the given field from this record
	 */
	
	def removeField(field : JawaField) = {
	  if(!field.isDeclared || field.getDeclaringClass != this) throw new RuntimeException("did not declare: " + field.getName)
	  this.fields -= field
	  field.clearDeclaringClass
	}
	
	/**
	 * get field from this record by the given name
	 */
	
	def getFieldByName(name : String) : JawaField = {
	  val fopt = getFields.find(_.getName == name)
	  fopt match{
	    case Some(f) => f
	    case None => 
        if(isUnknown){
          val f = new JawaField().init(name, StringFormConverter.getTypeFromName(Center.DEFAULT_TOPLEVEL_OBJECT), AccessFlag.getAccessFlags("PUBLIC"))
          addField(f)
          f
        } else throw new RuntimeException("No field " + name + " in record " + getName)
	  }
	}
	
	/**
	 * get field from this record by the given signature
	 */
	
	def getField(sig : String) : JawaField = {
	  val fieldName = StringFormConverter.getFieldNameFromFieldSignature(sig)
	  getFieldByName(fieldName)
	}
	
	/**
	 * get procedure from this record by the given subsignature
	 */
	
	def getMethod(subSig : String) : JawaMethod = {
	  tryGetMethod(subSig) match{
      case Some(p) => p
      case None => 
        if(isUnknown){
          val sig = StringFormConverter.getSigFromOwnerAndMethodSubSig(getName, subSig)
          val proc = new JawaMethod().init(sig)
          addMethod(proc)
          proc
        } else throw new RuntimeException("No procedure " + subSig + " in record " + getName)
    }
	}
	
	/**
	 * try to get procedure from this record by the given subsignature
	 */
	
	def tryGetMethod(subSig : String) : Option[JawaMethod] = {
	  this.subSigToMethods.get(subSig)
	}
	
	/**
	 * get procedure from this record by the given subsignature
	 */
	
	def getMethodByName(procName : String) : JawaMethod = {
	  if(!declaresMethodByName(procName)) throw new RuntimeException("No procedure " + procName + " in record " + getName)
	  var found = false
	  var foundMethod : JawaMethod = null
	  getMethods.foreach{
	    proc=>
	      if(proc.getName == procName){
	        if(found) throw new RuntimeException("ambiguous procedure" + procName)
	        else {
	          found = true
	          foundMethod = proc
	        }
	      }
	  }
	  if(found) foundMethod
	  else throw new RuntimeException("couldn't find method " + procName + "(*) in " + this)
	}
	
	/**
	 * get procedure from this record by the given subsignature
	 */
	
	def getMethodByShortName(procShortName : String) : JawaMethod = {
	  if(!declaresMethodByShortName(procShortName)) throw new RuntimeException("No procedure " + procShortName + " in record " + getName)
	  var found = false
	  var foundMethod : JawaMethod = null
	  getMethods.foreach{
	    proc=>
	      if(proc.getShortName == procShortName){
	        if(found) throw new RuntimeException("ambiguous procedure " + procShortName)
	        else {
	          found = true
	          foundMethod = proc
	        }
	      }
	  }
	  if(found) foundMethod
	  else throw new RuntimeException("couldn't find method " + procShortName + "(*) in " + this)
	}
	
	/**
	 * get procedure from this record by the given procedure name
	 */
	
	def getMethodsByName(procName : String) : Set[JawaMethod] = {
	  getMethods.filter(proc=> proc.getName == procName)
	}
	
	/**
	 * get procedure from this record by the given short proc name
	 */
	
	def getMethodsByShortName(procShortName : String) : Set[JawaMethod] = {
	  getMethods.filter(proc=> proc.getShortName == procShortName)
	}
	
	/**
	 * get static initializer of this record
	 */
	
	def getStaticInitializer : JawaMethod = getMethodByShortName(this.staticInitializerName)
	
	/**
	 * whether this procedure exists in the record or not
	 */
	
	def declaresMethod(subSig : String) : Boolean = this.subSigToMethods.contains(subSig)
	
	/**
	 * get procedure size of this record
	 */
	
	def getMethodSize : Int = this.procedures.size
	
	/**
	 * get procedures of this record
	 */
	
	def getMethods = this.procedures
	
	/**
	 * get procedure by the given name, parameter types and return type
	 */
	
	def getMethod(name : String, paramTyps : List[String], returnTyp : Type) : JawaMethod = {
	  var ap : JawaMethod = null
	  this.procedures.foreach{
	    proc=>
	      if(proc.getName == name && proc.getParamTypes == paramTyps && proc.getReturnType == returnTyp) ap = proc
	  }
	  if(ap == null) throw new RuntimeException("In " + getName + " does not have procedure " + name + "(" + paramTyps + ")" + returnTyp)
	  else ap
	}
	
	/**
	 * does procedure exist with the given name, parameter types and return type?
	 */
	
	def declaresMethod(name : String, paramTyps : List[String], returnTyp : Type) : Boolean = {
	  var find : Boolean = false
	  this.procedures.foreach{
	    proc=>
	      if(proc.getName == name && proc.getParamTypes == paramTyps && proc.getReturnType == returnTyp) find = true
	  }
	  find
	}
	
	/**
	 * does procedure exist with the given name and parameter types?
	 */
	
	def declaresMethod(name : String, paramTyps : List[String]) : Boolean = {
	  var find : Boolean = false
	  this.procedures.foreach{
	    proc=>
	      if(proc.getName == name && proc.getParamTypes == paramTyps) find = true
	  }
	  find
	}
	
	/**
	 * does procedure exists with the given name?
	 */
	
	def declaresMethodByName(name : String) : Boolean = {
	  var find : Boolean = false
	  this.procedures.foreach{
	    proc=>
	      if(proc.getName == name) find = true
	  }
	  find
	}
	
	/**
	 * does procedure exists with the given short name?
	 */
	
	def declaresMethodByShortName(name : String) : Boolean = {
	  var find : Boolean = false
	  this.procedures.foreach{
	    proc=>
	      if(proc.getShortName == name) find = true
	  }
	  find
	}
	
	/**
	 * return true if this record has static initializer
	 */
	
	def declaresStaticInitializer : Boolean = declaresMethodByShortName(this.staticInitializerName)
	
	/**
	 * add the given procedure to this record
	 */
	
	def addMethod(ap : JawaMethod) = {
	  if(ap.isDeclared) throw new RuntimeException(ap.getName + " is already declared in record " + ap.getDeclaringClass.getName)

	  if(this.subSigToMethods.contains(ap.getSubSignature)) throw new RuntimeException("The procedure " + ap.getName + " is already declared in record " + getName)
	  this.subSigToMethods += (ap.getSubSignature -> ap)
	  this.procedures += ap
	  ap.setDeclaringClass(this)
	}
	
	/**
	 * remove the given procedure from this record
	 */
	
	def removeMethod(ap : JawaMethod) = {
	  if(!ap.isDeclared || ap.getDeclaringClass != this) throw new RuntimeException("Not correct declarer for remove: " + ap.getName)
	  if(!this.subSigToMethods.contains(ap.getSubSignature)) throw new RuntimeException("The procedure " + ap.getName + " is not declared in record " + getName)
	  this.subSigToMethods -= ap.getSubSignature
	  this.procedures -= ap
	  ap.clearDeclaringClass
	}
	
	/**
	 * get interface size
	 */
	
	def getInterfaceSize : Int = this.interfaces.size
	
	/**
	 * get interfaces
	 */
	
	def getInterfaces = this.interfaces
	
	/**
	 * whether this record implements the given interface
	 */
	
	def implementsInterface(name : String) : Boolean = {
	  this.interfaces.foreach{
	    interface =>
	      if(interface.getName == name) return true
	  }
	  false
	}
	
	/**
	 * add an interface which is directly implemented by this record
	 */
	
	def addInterface(i : JawaClass) = {
    if(!i.isInterface) throw new RuntimeException("This is not an interface:" + i)
	  this.interfaces += i
	}
	
	/**
	 * add an interface which is directly implemented by this record
	 */
	
	def addInterfaceCheck(i : JawaClass) = {
	  if(implementsInterface(i.getName)) throw new RuntimeException("already implements this interface: " + i.getName)
	  addInterface(i)
	}
	
	/**
	 * remove an interface from this record
	 */
	
	def removeInterface(i : JawaClass) = {
    if(!i.isInterface) throw new RuntimeException("This is not an interface:" + i)
    this.interfaces += i
	  if(implementsInterface(i.getName)) throw new RuntimeException("no such interface: " + i.getName)
	  this.interfaces -= i
	}
	
	/**
	 * whether the current record has a super class or not
	 */
	
	def hasSuperClass = this.superClass != null
	
	/**
	 * get the super class
	 */
	
	def getSuperClass : JawaClass = {
	  if(!hasSuperClass) throw new RuntimeException("no super class for: " + getName)
	  else this.superClass
	}
	
	/**
	 * try to get the super class
	 */
	
	def tryGetSuperClass : Option[JawaClass] = {
	  if(!hasSuperClass) None
	  else Some(this.superClass)
	}
	
	/**
	 * set the super class
	 */
	
	def setSuperClass(sc : JawaClass) = {
	  this.superClass = sc
	}
	
	/**
	 * whether the current record has an outer class or not
	 */
	
	def hasOuterClass = this.outerClass != null
	
	/**
	 * get the outer class
	 */
	
	def getOuterClass : JawaClass = {
	  if(!hasOuterClass) throw new RuntimeException("no outer class for: " + getName)
	  else this.outerClass
	}
	
	/**
	 * try to get the outer class
	 */
	
	def tryGetOuterClass : Option[JawaClass] = {
	  if(!hasOuterClass) None
	  else Some(this.outerClass)
	}
	
	/**
	 * set outer class
	 */
	
	def setOuterClass(oc : JawaClass) = {
	  this.outerClass = oc
	}
	
	/**
	 * whether current record is an inner class or not
	 */
	
	def isInnerClass : Boolean = hasOuterClass
	
	/**
   * return true if this record is an interface
   */
  
  def isInterface : Boolean = AccessFlag.isInterface(this.accessFlags)
	
	/**
   * return true if this record is abstract
   */
  
  def isAbstract : Boolean = AccessFlag.isAbstract(this.accessFlags)
  
  /**
   * return true if this record is concrete
   */
  
  def isConcrete : Boolean = !isInterface && !isAbstract
  
  /**
   * return true if this record is public
   */
  
  def isPublic : Boolean = AccessFlag.isPublic(this.accessFlags)
  
  /**
   * return true if this record is private
   */
  
  def isPrivate : Boolean = AccessFlag.isPrivate(this.accessFlags)
  
  /**
   * return true if this record is protected
   */
  
  def isProtected : Boolean = AccessFlag.isProtected(this.accessFlags)
  
  /**
   * return true if this record is final
   */
  
  def isFinal : Boolean = AccessFlag.isFinal(this.accessFlags)
  
  /**
   * return true if this record is static
   */
  
  def isStatic : Boolean = AccessFlag.isStatic(this.accessFlags)
  
  /**
   * return true if it's a child of given record
   */
  
  def isChildOf(rec : JawaClass): Boolean = {
	  Center.getClassHierarchy.getAllSuperClassesOf(this).contains(rec)
	}
  
  /**
   * is this record an application record
   */
  
  def isApplicationClass : Boolean = Center.getApplicationClasses.contains(this)
  
  /**
   * set this record as an application record
   */
  
  def setApplicationClass = {
	  val c = Center.getContainingSet(this)
	  if(c != null) Center.removeFromContainingSet(this)
	  Center.addApplicationClass(this)
	}
	
	/**
   * is this record  a framework record
   */
  
  def isFrameworkClass : Boolean = Center.getFrameworkClasses.contains(this)
  
  /**
   * is this record  a third party lib record
   */
  
  def isThirdPartyLibClass : Boolean = Center.getThirdPartyLibClasses.contains(this)
  
  
  /**
   * set this record as a framework record
   */
  
  def setFrameworkClass = {
	  val c = Center.getContainingSet(this)
	  if(c != null) Center.removeFromContainingSet(this)
	  Center.addFrameworkClass(this)
	}
  
  /**
   * set this record as a third party lib record
   */
  
  def setThirdPartyLibClass = {
    val c = Center.getContainingSet(this)
    if(c != null) Center.removeFromContainingSet(this)
    Center.addThirdPartyLibClass(this)
  }
  
  /**
   * whether this record is a java library class
   */
  
  def isJavaLibraryClass : Boolean = 
    this.packageName.startsWith("java.") ||
    this.packageName.startsWith("sun.") ||
    this.packageName.startsWith("javax.") ||
    this.packageName.startsWith("com.sun.") ||
    this.packageName.startsWith("org.omg.") ||
    this.packageName.startsWith("org.xml.")
    
    
  /**
	 * retrieve code belong to this record
	 */
	
	def retrieveCode = JawaCodeSource.getClassCode(getName, Center.ResolveLevel.BODY)
	
	/**
	 * update resolving level for current record
	 */
	
	def updateResolvingLevel = {
    setResolvingLevel(getMethods.map(_.getResolvingLevel).reduceLeft((x, y) => if(x < y) x else y))
  }
	
	def printDetail = {
    println("++++++++++++++++AmandroidClass++++++++++++++++")
    println("recName: " + getName)
    println("packageName: " + getPackageName)
    println("shortName: " + getShortName)
    println("superClass: " + tryGetSuperClass)
    println("outerClass: " + tryGetOuterClass)
    println("interfaces: " + getInterfaces)
    println("accessFlags: " + getAccessFlagString)
    println("isInCenter: " + isInCenter)
    println("fields: " + getFields)
    println("procedures: " + getMethods)
    println("++++++++++++++++++++++++++++++++")
  }
	
  override def toString : String = getName
}