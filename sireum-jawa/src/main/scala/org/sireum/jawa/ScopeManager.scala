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
package org.sireum.jawa

import org.sireum.util.ISet

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait ScopeManager {
  /**
   * return true if given record needs to be bypassed
   */
  def shouldBypass(rec : JawaClass) : Boolean
}

class DefaultScopeManager extends ScopeManager {
  def shouldBypass(rec : JawaClass) : Boolean = false
}

object ScopeManager{
  private var currentScopeManager: ScopeManager = new DefaultScopeManager
  def setScopeManager(manager: ScopeManager) = this.currentScopeManager = manager
  def getCurrentScopeManager: ScopeManager = this.currentScopeManager
}
