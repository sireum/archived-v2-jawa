/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa

trait ResolveLevel {
	/**
   * resolving level of current record
   */
  
  protected var resolvingLevel : Center.ResolveLevel.Value = null
  
  /**
   * check whether we already resolved to desired level
   */
  
  def checkLevelAndThrowException(level : Center.ResolveLevel.Value, message : String) = {
    if(this.resolvingLevel < level) {
      val msg = "desired level: " + level + ". resolving level: " + this.resolvingLevel + " message: " + message 
      throw new RuntimeException(msg)
    }
  }
  
  /**
   * check whether we already resolved to desired level
   */
  
  def checkLevel(level : Center.ResolveLevel.Value) = this.resolvingLevel >= level
  
  /**
   * return resolving level
   */
  
  def getResolvingLevel = this.resolvingLevel
  
  /**
   * set resolving level
   */
  
  def setResolvingLevel(level : Center.ResolveLevel.Value) = {
    this.resolvingLevel = level
  }
}