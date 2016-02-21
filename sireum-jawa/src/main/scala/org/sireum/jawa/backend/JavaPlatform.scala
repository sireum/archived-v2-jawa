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
package org.sireum.jawa.backend

import org.sireum.jawa.Global
import org.sireum.jawa.classpath.MergedClasspath
import org.sireum.jawa.classpath.Classpath
import org.sireum.jawa.classpath.FlatClasspath
import org.sireum.jawa.io.AbstractFile
import org.sireum.jawa.classpath.DeltaClasspath
import org.sireum.jawa.classpath.PathResolver
import org.sireum.jawa.ClasspathRepresentationType
import org.sireum.jawa.classpath.FlatClasspathResolver


trait JavaPlatform extends Platform {
  val global: Global
  val javaLib: String
  import global._

  private[jawa] var currentClassPath: Option[MergedClasspath] = None

  def classPath: Classpath = {
    assert(classpathImpl == ClasspathRepresentationType.Recursive,
      "To use recursive classpath representation you must enable it with recursive compiler option.")

    if (currentClassPath.isEmpty) currentClassPath = Some(new PathResolver(javaLib).result)
    currentClassPath.get
  }

  private[jawa] lazy val flatClassPath: FlatClasspath = {
    assert(classpathImpl == ClasspathRepresentationType.Flat,
      "To use flat classpath representation you must enable it with flat compiler option.")

    new FlatClasspathResolver(javaLib).result
  }

  /** Update classpath with a substituted subentry */
  def updateClassPath(subst: Map[Classpath, Classpath]) =
    currentClassPath = Some(new DeltaClasspath(currentClassPath.get, subst))


  def needCompile(bin: AbstractFile, src: AbstractFile) =
    src.lastModified >= bin.lastModified
}
