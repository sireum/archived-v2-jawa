/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.util

import org.sireum.jawa.sjc.ObjectType

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait LibraryAPISummary {
  
  /**
   * check given API name is present in library
   */
	def isLibraryAPI(apiType: ObjectType): Boolean
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object DefaultLibraryAPISummary extends LibraryAPISummary {
  def isLibraryAPI(apiType: ObjectType): Boolean = false
}