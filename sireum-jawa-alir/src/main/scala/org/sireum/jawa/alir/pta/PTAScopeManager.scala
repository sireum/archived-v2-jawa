/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.jawa.alir.pta

import org.sireum.jawa.ScopeManager
import org.sireum.util._
import org.sireum.jawa.JawaClass

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object PTAScopeManager extends ScopeManager{
  private var passSystemLibrary = true
  private var passUserLibrary = true
  private var passApplication = false
  def setMode(passSystemLibrary: Boolean, passUserLibrary: Boolean, passApplication : Boolean) = {
    this.passSystemLibrary = passSystemLibrary
    this.passUserLibrary = passUserLibrary
    this.passApplication = passApplication
  }
  
  /**
   * return true if given record needs to be bypassed
   */
  def shouldBypass(rec : JawaClass) : Boolean = {
    if(this.passSystemLibrary && rec.isSystemLibraryClass) true
    else if(this.passUserLibrary && rec.isUserLibraryClass) true
    else if(this.passApplication && rec.isApplicationClass) true
    else false
  }
}
