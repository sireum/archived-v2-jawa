package org.sireum.jawa

import org.sireum.util._

trait JawaElement extends PropertyProvider {
  
  /**
   * supply property
   */
  val propertyMap = mlinkedMapEmpty[Property.Key, Any]
  
  def accessFlags: Int
  
  def getAccessFlags: Int = this.accessFlags
  
  /**
   * get field access flags in text form
   */
  def getAccessFlagsStr: String = AccessFlag.toString(this.accessFlags)
  
  /**
   * unknown means it's not available in our code repo
   */
  protected var unknown: Boolean = false
  
  def setUnknown = this.unknown = true
  
  def isConcrete: Boolean
  
  /**
   * return true if this class is abstract
   */
  def isAbstract: Boolean = AccessFlag.isAbstract(this.accessFlags)
  
  /**
   * return true if this class is public
   */
  def isPublic: Boolean = AccessFlag.isPublic(this.accessFlags)
  
  /**
   * return true if this class is private
   */
  def isPrivate: Boolean = AccessFlag.isPrivate(this.accessFlags)
  
  /**
   * return true if this class is protected
   */
  def isProtected: Boolean = AccessFlag.isProtected(this.accessFlags)
  
  /**
   * return true if this class is final
   */
  def isFinal: Boolean = AccessFlag.isFinal(this.accessFlags)
  
  /**
   * return true if this class is static
   */
  def isStatic: Boolean = AccessFlag.isStatic(this.accessFlags)
  
  /**
   * return true if this method is native
   */
  def isNative: Boolean = AccessFlag.isNative(this.accessFlags)
  
  /**
   * return true if this class is unknown class
   */
  def isUnknown: Boolean = this.unknown
  
  def isClass: Boolean = this.isInstanceOf[JawaClass]
  
  def isMethod: Boolean = this.isInstanceOf[JawaMethod]
  
  def isField: Boolean = this.isInstanceOf[JawaField]
}