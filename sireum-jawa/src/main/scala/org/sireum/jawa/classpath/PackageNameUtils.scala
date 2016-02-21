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
package org.sireum.jawa.classpath

import org.sireum.jawa.classpath.FlatClasspath.RootPackage

/**
 * Common methods related to package names represented as String
 */
object PackageNameUtils {

  /**
   * @param fullClassName full class name with package
   * @return (package, simple class name)
   */
  def separatePkgAndClassNames(fullClassName: String): (String, String) = {
    val lastDotIndex = fullClassName.lastIndexOf('.')
    if (lastDotIndex == -1)
      (RootPackage, fullClassName)
    else
      (fullClassName.substring(0, lastDotIndex), fullClassName.substring(lastDotIndex + 1))
  }

  def packagePrefix(inPackage: String): String = if (inPackage == RootPackage) "" else inPackage + "."
}
