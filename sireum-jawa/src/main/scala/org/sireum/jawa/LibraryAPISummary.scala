/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait LibraryAPISummary {
  
  /**
   * check given API name is present in library
   */
  def isLibraryAPI(apiName : String): Boolean
  
  def isLibraryClass(typ: JawaType): Boolean
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object DefaultLibraryAPISummary extends LibraryAPISummary {
  def isLibraryAPI(apiName : String) : Boolean = false
  def isLibraryClass(typ: JawaType): Boolean = false
}
