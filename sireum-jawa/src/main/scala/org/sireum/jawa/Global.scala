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
    this.cachedClassRepresentation.clear()
  }
  
}