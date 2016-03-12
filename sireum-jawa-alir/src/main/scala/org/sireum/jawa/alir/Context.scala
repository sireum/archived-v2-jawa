/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa.alir

import org.sireum.jawa.Signature
import org.sireum.util._

object Context {
  private var k: Int = 1
  def init_context_length(k: Int) = this.k = k
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
class Context {
  import Context._
  def copy: Context = {
    val clone = new Context
    clone.callStack ++= this.callStack
    clone
  }
//  def copy(c: Context) = this.callStack = c.getContext
  def setContext(callStack2: IList[(Signature, String)]) = {
    val size2 = callStack2.length
    callStack.prependAll(callStack2)
    val size = length
    if(size > k + 1) {
      callStack.remove(size - (k + 1))
    }
  }
  private val callStack: MList[(Signature, String)] = mlistEmpty
  def length: Int = this.callStack.size
  def setContext(pSig: Signature, loc: String) = {
    if(length <= k){
      callStack.prepend((pSig, loc))
    } else {
      callStack.remove(length - 1)
      callStack.prepend((pSig, loc))
    }
    this
  }
  
  def getCurrentLocUri: String = {
    if(callStack.isEmpty) ""
    else callStack.head._2
  }
  
  /**
   * update current context using another context.
   */
  def updateContext(context2: Context) = {
    val size2 = context2.length
    val callStack2 = context2.getContext
    callStack.prependAll(callStack2)
    val size = length
    if(size > k + 1) {
      callStack.remove(size - (k + 1))
    }
  }
  
  /**
   * remove current top context
   */
  def removeTopContext = {
    if(!callStack.isEmpty)
    	  callStack.remove(0)
    this
  }
  
  def getContext: IList[(Signature, String)] = this.callStack.toList
  def getLocUri: String = getContext(0)._2
  def getMethodSig = getContext(0)._1
  def isDiff(c: Context): Boolean = !this.callStack.equals(c.getContext)
  override def equals(a: Any): Boolean = {
    if(a.isInstanceOf[Context]) this.callStack.equals(a.asInstanceOf[Context].getContext)
    else false
  }
  override def hashCode() = if (this.callStack == null) 0 else this.callStack.hashCode
  override def toString = {
    var sb = new StringBuilder
    this.callStack.foreach{
      case(sig, str) =>
        sb.append("(" + sig.methodName)
          
        if(str.lastIndexOf('.') > 0)
          sb.append("," + str.substring(str.lastIndexOf('.') + 1, str.lastIndexOf(':')) + ")")
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
