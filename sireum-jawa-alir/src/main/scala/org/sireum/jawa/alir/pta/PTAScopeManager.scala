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
import org.sireum.jawa.JawaClass

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object PTAScopeManager extends ScopeManager{
  private var passFramework = true
  private var passThirdPartyLib = true
  private var passApplication = false
  def setMode(passFramework : Boolean, passThirdPartyLib : Boolean, passApplication : Boolean) = {
    this.passFramework = passFramework
    this.passThirdPartyLib = passThirdPartyLib
    this.passApplication = passApplication
  }
  
  /**
   * return true if given record needs to be bypassed
   */
  def shouldBypass(rec : JawaClass) : Boolean = {
    if(this.passFramework && rec.isFrameworkClass) true
    else if(this.passThirdPartyLib && rec.isThirdPartyLibClass) true
    else if(this.passApplication && rec.isApplicationClass) true
    else false
  }
}