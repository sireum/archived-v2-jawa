/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa

import org.sireum.util.ISet

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait ScopeManager {
  def setMode(inScopeMode : Boolean)
  
  /**
   * return true means use include mode, any package defined in ScopeManager will be keep
   * during the analysis, and vice versa.
   */
  def isIncludeMode : Boolean
	
  def addPackage(packageName : String)
  def addPackages(packageNames : ISet[String])
  def removePackage(packageName : String)
	def removePackages(packageNames : ISet[String])
	
	/**
	 * return true if given package name contained in the scope manager
	 */
  def contains(packageName : String) : Boolean
	/**
	 * return true if given record needs to be bypassed
	 */
  def shouldBypass(rec : JawaRecord) : Boolean
  def clear
}