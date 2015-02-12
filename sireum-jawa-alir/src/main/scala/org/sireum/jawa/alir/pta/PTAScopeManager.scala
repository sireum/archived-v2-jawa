/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.pta

import org.sireum.jawa.ScopeManager
import org.sireum.util._
import org.sireum.jawa.JawaRecord

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object PTAScopeManager extends ScopeManager{
  private var packages : ISet[String] = isetEmpty
  private var includeMode = true
  private var passFramework = true
  private var passThirdPartyLib = true
  private var passApplication = false
  def setMode(includeMode : Boolean, passFramework : Boolean, passThirdPartyLib : Boolean, passApplication : Boolean) = {
    this.includeMode = includeMode
    this.passFramework = passFramework
    this.passThirdPartyLib = passThirdPartyLib
    this.passApplication = passApplication
  }
  /**
   * return true means use in scope mode, any package defined in ScopeManager will be keep
   * during the analysis, and vice versa.
   */
  def isIncludeMode : Boolean = this.includeMode
  
  def addPackage(packageName : String) = this.packages += packageName
  def addPackages(packageNames : ISet[String]) = this.packages ++= packageNames
  def removePackage(packageName : String) = this.packages -= packageName
  def removePackages(packageNames : ISet[String]) = this.packages --= packageNames
  
  /**
   * return true if given package name contained in the scope manager
   */
  def contains(packageName : String) : Boolean = this.packages.contains(packageName)
  def clear = this.packages = isetEmpty
  
  /**
   * return true if given record needs to be bypassed
   */
  def shouldBypass(rec : JawaRecord) : Boolean = {
    if(this.passFramework && rec.isFrameworkRecord) true
    else if(this.passThirdPartyLib && rec.isThirdPartyLibRecord) true
    else if(this.passApplication && rec.isApplicationRecord) true
    else if(isIncludeMode){
      if(rec.getPackageName != null) !contains(rec.getPackageName) else true
    } else {
      if(rec.getPackageName != null) contains(rec.getPackageName) else false
    }
  }
}