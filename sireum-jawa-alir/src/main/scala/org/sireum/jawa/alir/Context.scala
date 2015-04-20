/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class Context(var k : Int){
  def copy : Context = {
    val clone = new Context(k)
    clone.callStack ++= this.callStack
    clone
  }
  def copy(c : Context) = this.callStack = c.getContext
  private var callStack : List[(String, String)] = List()
  def length : Int = this.callStack.size
  def setContext(pSig : String, loc : String) = {
    if(length <= k){
      callStack = (pSig, loc) :: callStack
    } else {
      callStack = (pSig, loc) :: callStack.dropRight(1)
    }
    this
  }
  
  def getCurrentLocUri : String = {
    if(callStack.isEmpty) ""
    else callStack.head._2
  }
  
  /**
   * update current context using another context.
   */
  def updateContext(context2 : Context) = {
    this.k = context2.k
    val size2 = context2.length
    val callStack2 = context2.getContext
    val size = length
    if(size + size2 <= k + 1){
      callStack = callStack ::: callStack2
    } else {
      callStack = callStack ::: callStack2.dropRight(size2 - (k + 1 - size))
    }
  }
  
  /**
   * remove current top context
   */
  def removeTopContext = {
    if(!callStack.isEmpty)
    	callStack = callStack.tail
    this
  }
  
  def getContext : List[(String, String)] = this.callStack
  def getLocUri : String = getContext(0)._2
  def getMethodSig = getContext(0)._1
  def isDiff(c : Context) : Boolean = !this.callStack.equals(c.getContext)
  override def equals(a : Any) : Boolean = {
    if(a.isInstanceOf[Context]) this.callStack.equals(a.asInstanceOf[Context].getContext)
    else false
  }
  override def hashCode() = if (this.callStack == null) 0 else this.callStack.hashCode
  override def toString = {
    var sb = new StringBuilder
    this.callStack.foreach{
      case(sig, str) =>
        if(sig.lastIndexOf('.') > 0 && sig.lastIndexOf(':') > 0)
          sb.append("(" + sig.substring(sig.lastIndexOf('.') + 1, sig.lastIndexOf(':')))
        else
          sb.append("(" + sig)
          
        if(str.lastIndexOf('.') > 0)
        	sb.append("," + str.substring(sig.lastIndexOf('.') + 1, sig.lastIndexOf(':')) + ")")
        else sb.append("," + str + ")")
    }
    sb.toString.intern()
  }
  def toFullString = {
    var sb = new StringBuilder
    this.callStack.foreach{
      case(sig, str) =>
        sb.append("(" + sig)
        sb.append("," + str + ")")
    }
    sb.toString.intern()
  }
}