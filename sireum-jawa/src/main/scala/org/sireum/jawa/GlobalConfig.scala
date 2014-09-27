/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa

import org.sireum.util.FileUtil
import java.io.File

/**
 * All the global config's settings at the beginning
 */

object GlobalConfig {
  
  final val PILAR_FILE_EXT = ".pilar"
	
	/**
	 * is whole program mode or not
	 */
	final var mode : Mode.Value = Mode.APP_ONLY
	
	/**
	 * call graph context length
	 */
	final var CG_CONTEXT_K = 1
	
	/**
	 * switch for jawa resolver parallel
	 */
	final var jawaResolverParallel = false
	
	/**
	 * switch for android info collect parallel
	 */
	final var androidInfoCollectParallel = true
}

/**
 * APP_ONLY_TEST mode: processes app but doesn't go inside library, doesn't require code to be complete
 * APP_ONLY: processes app but doesn't go inside library, requires code to be complete
 * WHOLE_PROGRAM_TEST mode: processes app and go inside library, doesn't require code to be complete
 * WHOLE_PROGRAM mode: processes app and go inside library, requires code to be complete
 */

object Mode extends Enumeration {
  val APP_ONLY, WHOLE_PROGRAM = Value
}