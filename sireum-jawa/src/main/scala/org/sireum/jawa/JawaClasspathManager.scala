/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa

import org.sireum.util._
import org.sireum.jawa.backend.JavaPlatform
import org.sireum.jawa.classpath.Classpath
import org.sireum.jawa.classpath.ClassFileLookup
import org.sireum.jawa.classpath.FlatClasspath

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait JawaClasspathManager extends JavaKnowledge { self: Global =>
  
  private final val TITLE = "JawaClasspathManager"
  
  // platform specific elements

  private var javaLibrary: String = ""
  
  def setJavaLib(path: String) = javaLibrary = path
  
  protected class GlobalPlatform extends {
    val global: this.type = this
    val javaLib: String = javaLibrary
  } with JavaPlatform

  type ThisPlatform = JavaPlatform { val global: this.type }
  lazy val platform = new GlobalPlatform

  type PlatformClassPath = Classpath
  type OptClassPath = Option[PlatformClassPath]
  
  def classpathImpl: ClasspathRepresentationType.Value = ClasspathRepresentationType.Flat
  
  def classPath: ClassFileLookup = classpathImpl match {
    case ClasspathRepresentationType.Flat => flatClassPath
    case ClasspathRepresentationType.Recursive => recursiveClasspath
  }

  private def recursiveClasspath: Classpath = platform.classPath

  private def flatClassPath: FlatClasspath = platform.flatClassPath
  
}