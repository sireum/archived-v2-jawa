/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa.alir.taintAnalysis

import org.sireum.util._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class TaintTag {
  private val tags : MSet[String] = msetEmpty
  
  /**
   * tagStr : Taint tag has to be following format: WEB|XSS|LOCATION ...
   */
  def setTags(tagStr : String) = {
    val ts = tagStr.split("|")
    this.tags ++= ts
  }
  
  def getTags : Set[String] = {
    this.tags.toSet
  }
  
  /**
   * str : format --> +WEB+XSS-LOCATION ...
   */
  def parse(str : String) = {
    val it = str.iterator
    val sb = new StringBuilder
    var flag = 0
    while(it.hasNext){
      val s = it.next()
      if(flag != 0){
        if(s == ' ' || s == '+' || s == '-' || !it.hasNext){
          if(!it.hasNext) sb.append(s)
          println(sb.toString)
          if(flag == 1) tags += sb.toString()
          else if(flag == 2) tags -= sb.toString()
          sb.clear()
          flag = 0
        }
        else sb.append(s)
      }
      if(s == '+') flag = 1
      else if(s == '-') flag = 2
    }
  }
  
  override def toString : String = {
    val sb = new StringBuilder
    tags.foreach{
      tag =>
        sb.append(tag + " ")
    }
    sb.toString()
  }
}
