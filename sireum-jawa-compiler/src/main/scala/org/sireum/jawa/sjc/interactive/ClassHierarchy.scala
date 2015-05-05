/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.interactive

import org.sireum.util._
import org.sireum.jawa.sjc.AccessFlag
import org.sireum.jawa.sjc.JavaKnowledge

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
class ClassHierarchy extends JavaKnowledge {
	/**
	 * this map is from class to it's sub-classes.
	 */
  protected val classToSubClasses: MMap[JawaClass, MSet[JawaClass]] = mmapEmpty
  
  /**
   * this map is from interface to sub-interfaces.
   */
  protected val interfaceToSubInterfaces: MMap[JawaClass, MSet[JawaClass]] = mmapEmpty
  
  /**
   * this map is from class to all sub-classes.  Not filled in inside the build()
   */
  protected val classToAllSubClasses: MMap[JawaClass, MSet[JawaClass]] = mmapEmpty
  
  /**
   * this map is from interface to all sub-interfaces. Not filled in inside the build()
   */
  protected val interfaceToAllSubInterfaces: MMap[JawaClass, MSet[JawaClass]] = mmapEmpty
  
  /**
   * this map is from interface to direct implementers
   */
  protected val interfaceToImplememters: MMap[JawaClass, MSet[JawaClass]] = mmapEmpty
  
  /**
   * construct a hierarchy from the current scene i.e. Global
   */
  def build(global: Global): ClassHierarchy = {
    val allClasses = global.getClassesNeedUpdateInHierarchy
    allClasses.foreach{
      clazz =>
        if(clazz.hasSuperClass){
          if(clazz.isInterface){
            clazz.getInterfaces.foreach{this.interfaceToSubInterfaces.getOrElseUpdate(_, msetEmpty) += clazz}
          } else {
            this.classToSubClasses.getOrElseUpdate(clazz.getSuperClass, msetEmpty) += clazz
            clazz.getInterfaces.foreach{this.interfaceToImplememters.getOrElseUpdate(_, msetEmpty) += clazz}
          }
        }
    }
    // fill in the implementers sets with subclasses
    allClasses.foreach{
      clazz =>
        if(clazz.isInterface){
          val imps = this.interfaceToImplememters.getOrElseUpdate(clazz, msetEmpty)
          if(!imps.isEmpty)
          	imps ++= imps.map{getAllSubClassesOfIncluding(_)}.reduce((s1, s2) => s1 ++ s2)
        }
    }
    global.clearClassesNeedUpdateInHierarchy
    this
  }
  
  /**
   * return a set of all sub-classes of r, including itself
   */
  def getAllSubClassesOfIncluding(r: JawaClass): Set[JawaClass] = {
    if(r.isInterface) throw new RuntimeException("r need to be class type: " + r)
    getAllSubClassesOf(r) + r
  }
  
  /**
   * return a set of all sub-classes of r
   */
  def getAllSubClassesOf(r: JawaClass): Set[JawaClass] = {
    if(r.isInterface) throw new RuntimeException("r need to be class type: " + r)
    this.classToAllSubClasses.get(r) match{
      case Some(classes) => classes.toSet //if already cached return the value
      case None => 
        val subClasses = this.classToSubClasses.getOrElseUpdate(r, msetEmpty)
        if(!subClasses.isEmpty){
	        val allSubClasses = subClasses.map{getAllSubClassesOfIncluding(_)}.reduce((s1, s2) => s1 ++ s2)
	        this.classToAllSubClasses.getOrElseUpdate(r, msetEmpty) ++= allSubClasses
	        allSubClasses
        } else Set()
    }
  }
  
  /**
   * return a set of all super-classes of r, including itself
   */
  def getAllSuperClassesOfIncluding(r: JawaClass): Set[JawaClass] = {
    if(r.isInterface) throw new RuntimeException("r need to be class type: " + r)
    getAllSuperClassesOf(r) + r
  }
  
  /**
   * return a set of all super-classes of r
   */
  def getAllSuperClassesOf(r: JawaClass): Set[JawaClass] = {
    if(r.isInterface) throw new RuntimeException("r need to be class type: " + r)
    var rl = r
    var l: Set[JawaClass] = Set()
    while(rl.hasSuperClass){
      l += rl.getSuperClass
      rl = rl.getSuperClass
    }
    l
  }
  
  /**
   * return a set of all sub-interfaces of r, including itself
   */
  def getAllSubInterfacesOfIncluding(r: JawaClass): Set[JawaClass] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    getAllSubInterfacesOf(r) + r
  }
  
  /**
   * return a set of all sub-interfaces of r
   */
  def getAllSubInterfacesOf(r: JawaClass): Set[JawaClass] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    this.interfaceToAllSubInterfaces.get(r) match{
      case Some(classes) => classes.toSet //if already cached return the value
      case None => 
        val subClasses = this.interfaceToSubInterfaces.getOrElseUpdate(r, msetEmpty)
        if(!subClasses.isEmpty){
	        val allSubClasses = subClasses.map{getAllSubInterfacesOfIncluding(_)}.reduce((s1, s2) => s1 ++ s2)
	        this.interfaceToAllSubInterfaces.getOrElseUpdate(r, msetEmpty) ++= allSubClasses
	        allSubClasses
        } else Set()
    }
  }
  
  /**
   * return a set of all super-interfaces of r, including itself
   */
  def getAllSuperInterfacesOfIncluding(r: JawaClass): Set[JawaClass] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    getAllSuperInterfacesOf(r) + r
  }
  
  /**
   * return a set of all super-interfaces of r
   */
  def getAllSuperInterfacesOf(r: JawaClass): Set[JawaClass] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    val ins = r.getInterfaces
    if(!ins.isEmpty)
    	ins.map{getAllSuperInterfacesOf(_)}.reduce((s1, s2) => s1 ++ s2) ++ ins
    else
      ins
  }
  
  /**
   * return a set of sub-classes of r, including itself
   */
  def getSubClassesOfIncluding(r: JawaClass): Set[JawaClass] = {
    if(r.isInterface) throw new RuntimeException("r need to be class type: " + r)
    getSubClassesOf(r) + r
  }
  
  /**
   * return a set of sub-classes of r
   */
  def getSubClassesOf(r: JawaClass): Set[JawaClass] = {
    if(r.isInterface) throw new RuntimeException("r need to be class type: " + r)
    this.classToSubClasses.getOrElse(r, msetEmpty).toSet
  }
  
  /**
   * return super-classe of r
   */
  def getSuperClassOf(r: JawaClass): JawaClass = {
    if(r.isInterface) throw new RuntimeException("r need to be class type: " + r)
    r.getSuperClass
  }
  
  /**
   * return a set of sub-interfaces of r, including itself
   */
  def getSubInterfacesOfIncluding(r: JawaClass): Set[JawaClass] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    getSubInterfacesOf(r) + r
  }
  
  /**
   * return a set of sub-interfaces of r
   */
  def getSubInterfacesOf(r: JawaClass): Set[JawaClass] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    this.interfaceToSubInterfaces.getOrElse(r, msetEmpty).toSet
  }
  
  /**
   * return a set of all super-interfaces of r
   */
  def getSuperInterfacesOf(r: JawaClass): Set[JawaClass] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    r.getInterfaces
  }
  
  /**
   * get all implementers of r
   */
  def getAllImplementersOf(r: JawaClass): Set[JawaClass] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    val subI = getSubInterfacesOfIncluding(r)
    if(!subI.isEmpty)
    	subI.map{getImplementersOf(_)}.reduce((s1, s2) => s1 ++ s2)
    else Set()
  }
  
  /**
   * get implementers of r
   */
  def getImplementersOf(r: JawaClass): Set[JawaClass] = {
    if(!r.isInterface) throw new RuntimeException("r need to be interface type: " + r)
    this.interfaceToImplememters.getOrElse(r, msetEmpty).toSet
  }
  
  /**
   * return true if child is a subclass of given parent recursively
   */
  def isClassRecursivelySubClassOf(child: JawaClass, parent: JawaClass): Boolean = {
    getAllSuperClassesOf(child).contains(parent)
  }
  
   /**
   * return true if child is a subclass of given parent recursively
   */
  def isClassRecursivelySubClassOfIncluding(child: JawaClass, parent: JawaClass): Boolean = {
    getAllSuperClassesOfIncluding(child).contains(parent)
  }
  
  /**
   * return true if child is a subclass of given parent
   */
  def isClassSubClassOf(child: JawaClass, parent: JawaClass): Boolean = {
    if(child.isInterface) throw new RuntimeException("r need to be class type: " + child)
    getSuperClassOf(child) == parent
  }
  
  /**
   * return true if child is a super class of given parent recursively
   */
  def isClassRecursivelySuperClassOf(parent: JawaClass, child: JawaClass): Boolean = {
    getAllSubClassesOf(parent).contains(child)
  }
  
  /**
   * return true if child is a super class of given parent recursively
   */
  def isClassRecursivelySuperClassOfIncluding(parent: JawaClass, child: JawaClass): Boolean = {
    getAllSubClassesOfIncluding(parent).contains(child)
  }
  
  /**
   * return true if child is a subclass of given parent
   */
  def isClassSuperClassOf(parent: JawaClass, child: JawaClass): Boolean = {
    if(parent.isInterface) throw new RuntimeException("r need to be class type: " + parent)
    child.getSuperClass == parent
  }
  
  /**
   * return true if child is a subinterface of given parent recursively
   */
  def isClassRecursivelySubInterfaceOf(child: JawaClass, parent: JawaClass): Boolean = {
    if(!child.isInterface) throw new RuntimeException("r need to be interface type: " + child)
    getAllSuperInterfacesOf(child).contains(parent)
  }
  
   /**
   * return true if child is a subinterface of given parent recursively
   */
  def isClassRecursivelySubInterfaceOfIncluding(child: JawaClass, parent: JawaClass): Boolean = {
    if(!child.isInterface) throw new RuntimeException("r need to be interface type: " + child)
    getAllSuperInterfacesOfIncluding(child).contains(parent)
  }
  
  /**
   * return true if child is a subinterface of given parent
   */
  def isClassSubInterfaceOf(child: JawaClass, parent: JawaClass): Boolean = {
    if(!child.isInterface) throw new RuntimeException("r need to be interface type: " + child)
    getSuperInterfacesOf(child).contains(parent)
  }
  
  /**
   * return true if the procedure is visible from record from
   */
  def isMethodVisible(from: JawaClass, method: JawaMethod): Boolean = {
    if(method.isUnknown) true
    else if(method.isPublic) true
    else if(method.isPrivate) method.getDeclaringClass == from
    else if(method.isProtected) isClassRecursivelySubClassOfIncluding(from, method.getDeclaringClass)
    /* If none of these access control accesflag been set, means the method has default or package level access
     * which means this method can be accessed within the class or other classes in the same package.
     */
    else method.getDeclaringClass == from || method.getDeclaringClass.getPackage == from.getPackage
  }
  
  /**
   * Given an object created by o = new R as type R, return the procedure which will be called by o.p()
   */
  def resolveConcreteDispatch(concreteType: JawaClass, p: JawaMethod): JawaMethod = {
    if(concreteType.isInterface) throw new RuntimeException("concreteType need to be class type: " + concreteType)
    val pSubSig = p.getSubSignature
    resolveConcreteDispatch(concreteType, pSubSig)
  }
  
  /**
   * Given an object created by o = new R as type R, return the procedure which will be called by o.p()
   */
  def resolveConcreteDispatch(concreteType: JawaClass, pSubSig: String): JawaMethod = {
    if(concreteType.isInterface) throw new RuntimeException("Receiver need to be class type: " + concreteType)
    findMethodThroughHierarchy(concreteType, pSubSig) match {
      case Some(ap) => 
        if(ap.isAbstract) throw new RuntimeException("Target procedure needs to be non-abstract method type: " + ap)
        else if(!isMethodVisible(concreteType, ap)) throw MethodInvisibleException("Target procedure " + ap + " needs to be visible from: " + concreteType)
        else ap
      case None => throw new RuntimeException("Cannot resolve concrete dispatch!\n" + "Type:" + concreteType + "\nMethod:" + pSubSig)
    }
  }
  
  private def findMethodThroughHierarchy(record: JawaClass, subSig: String): Option[JawaMethod] = {
    if(record.isUnknown){
      this.synchronized{
	      record.tryGetMethod(subSig) match{
	        case Some(p) => Some(p)
	        case None =>
            val unknownSig = generateSignatureFromOwnerAndMethodSubSignature(record, subSig)
	          val unknownMethod = generateUnknownJawaMethod(record, unknownSig)
	          Some(unknownMethod)
	      }
      }
    } else {
	    record.tryGetMethod(subSig) match{
	      case Some(p) =>
	        Some(p)
	      case None =>
	        if(record.hasSuperClass)
	        	findMethodThroughHierarchy(record.getSuperClass, subSig)
	        else None
	    }
    }
  }
  
  /**
   * Given an abstract dispatch to an object of type r and a procedure p, gives a list of possible receiver's methods
   */
  def resolveAbstractDispatch(r: JawaClass, pSubSig: String): Set[JawaMethod] = {
    val results: MSet[JawaMethod] = msetEmpty
    val classes: MSet[JawaClass] = msetEmpty
    if(r.isInterface){
      classes ++= getAllImplementersOf(r)
    } else {
      classes ++= getAllSubClassesOfIncluding(r)
    }
    
    classes.filter { r => !r.isAbstract }.foreach{
      rec =>
        findMethodThroughHierarchy(rec, pSubSig) match {
          case Some(p) => if(!p.isAbstract) results += p
          case None =>
        }
    }
    if(results.isEmpty){
      if(r.isInterface || r.isAbstract){
        findMethodThroughHierarchy(r, pSubSig) match { //check whether this method is in the java.lang.Object class.
          case Some(p) => if(!p.isAbstract) results += p
          case None => // It's an unknown method since we cannot find any implementer of this interface and such method is getting invoked.
        }
        if(results.isEmpty){
          val unknownrec = new JawaClass(r.global, r.getType.toUnknown, "PUBLIC")
          unknownrec.setApplicationClass
          unknownrec.setUnknown
          if(r.isInterface) unknownrec.addInterface(r)
          else if(r.isAbstract) unknownrec.setSuperClass(r)
          val unknownSig = generateSignatureFromOwnerAndMethodSubSignature(unknownrec, pSubSig)
          val unknownMethod = generateUnknownJawaMethod(unknownrec, unknownSig)
          results += unknownMethod
        }
      } else throw new RuntimeException("Could not resolve abstract dispath for:\nclass:" + r + " method:" + pSubSig)
    }
    results.toSet
  }
  
  /**
   * Given an abstract dispatch to an object of type r and a procedure p, gives a list of possible receiver's methods
   */
  def resolveAbstractDispatch(r: JawaClass, p: JawaMethod): Set[JawaMethod] = {
    val pSubSig = p.getSubSignature
    resolveAbstractDispatch(r, pSubSig)
  }
  
  def reset = {
    this.classToAllSubClasses.clear()
    this.classToSubClasses.clear()
    this.interfaceToAllSubInterfaces.clear()
    this.interfaceToImplememters.clear()
    this.interfaceToSubInterfaces.clear()
  }
  
  def printDetails = {
    println("==================hierarchy==================")
    println("interfaceToSubInterfaces:\n" + this.interfaceToSubInterfaces)
    println("classToSubClasses:\n" + this.classToSubClasses)
    println("interfaceToImplememters:\n" + this.interfaceToImplememters)
    println("====================================")
  }
  
  override def toString(): String = {
    val sb = new StringBuffer
    sb.append("\ninterface to sub-interfaces:\n")
    this.interfaceToSubInterfaces.foreach{
      case (k, v) =>
        sb.append(k + "->" + v + "\n")
    }
    sb.append("interface to implementers:\n")
    this.interfaceToImplememters.foreach{
      case (k, v) =>
        sb.append(k + "->" + v + "\n")
    }
    sb.append("class to sub-classes:\n")
    this.classToSubClasses.foreach{
      case (k, v) =>
        sb.append(k + "->" + v + "\n")
    }
    sb.toString().intern()
  }
}

case class MethodInvisibleException(detailMessage: String) extends RuntimeException