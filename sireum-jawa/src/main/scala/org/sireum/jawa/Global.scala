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

import org.sireum.jawa.classpath.ClassFileLookup
import org.sireum.jawa.classpath.FlatClasspath
import org.sireum.jawa.classpath.Classpath
import org.sireum.jawa.backend.JavaPlatform

object ClasspathRepresentationType extends Enumeration {
  val Flat, Recursive = Value
}

/**
 * @author fgwei
 */
class Global(val projectName: String, val reporter: Reporter) extends JawaClassLoadManager
  with JawaClasspathManager {
  
  /**
   * reset the current Global
   */
  def reset(removeCode: Boolean = true) = {
    this.classes.clear()
    this.applicationClasses.clear()
    this.systemLibraryClasses.clear()
    this.userLibraryClasses.clear()
    this.hierarchy.reset
    if(removeCode) {
      this.applicationClassCodes.clear()
      this.userLibraryClassCodes.clear()
    }
    this.cachedClassRepresentation.invalidateAll()
    this.classCache.invalidateAll()
    this.methodCache.invalidateAll()
  }
  
}
