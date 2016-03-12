/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa.classpath

import java.io.{ File => JFile }
import java.net.URL
import org.sireum.jawa.io.AbstractFile
import org.sireum.jawa.FatalError

/**
 * Common methods related to Java files and abstract files used in the context of classpath
 */
object FileUtils {
  implicit class AbstractFileOps(val file: AbstractFile) extends AnyVal {
    def isPackage: Boolean = file.isDirectory && mayBeValidPackage(file.name)

    def isClass: Boolean = !file.isDirectory && file.hasExtension("class")

    def isJavaSource: Boolean = !file.isDirectory && file.hasExtension("java")
    
    def isPilarSource: Boolean = !file.isDirectory && (file.hasExtension("pilar") || file.hasExtension("plr"))

    def isJarOrZip: Boolean = file.hasExtension("jar") || file.hasExtension("zip")
    
    def isPilarOrJavaSource: Boolean = isJavaSource || isPilarSource

    /**
     * Safe method returning a sequence containing one URL representing this file, when underlying file exists,
     * and returning given default value in other case
     */
    def toURLs(default: => Seq[URL] = Seq.empty): Seq[URL] = if (file.file == null) default else Seq(file.toURL)
  }

  implicit class FileOps(val file: JFile) extends AnyVal {
    def isPackage: Boolean = file.isDirectory && mayBeValidPackage(file.getName)

    def isClass: Boolean = file.isFile && file.getName.endsWith(".class")
  }

  def stripSourceExtension(fileName: String): String = {
    if (endsJava(fileName)) stripJavaExtension(fileName)
    else throw new FatalError("Unexpected source file ending: " + fileName)
  }

  def dirPath(forPackage: String) = forPackage.replace('.', '/')

  def endsClass(fileName: String): Boolean =
    fileName.length > 6 && fileName.substring(fileName.length - 6) == ".class"

  def endsPilarOrJava(fileName: String): Boolean = endsPilar(fileName) || endsPilarShort(fileName) || endsJava(fileName)
    
  def endsJava(fileName: String): Boolean =
    fileName.length > 5 && fileName.substring(fileName.length - 5) == ".java"

  def endsPilar(fileName: String): Boolean =
    fileName.length > 6 && (fileName.substring(fileName.length - 6) == ".pilar")
    
  def endsPilarShort(fileName: String): Boolean =
    fileName.length > 6 && (fileName.substring(fileName.length - 6) == ".plr")
    
  def stripClassExtension(fileName: String): String =
    fileName.substring(0, fileName.length - 6) // equivalent of fileName.length - ".class".length

  def stripJavaExtension(fileName: String): String =
    fileName.substring(0, fileName.length - 5)

  // probably it should match a pattern like [a-z_]{1}[a-z0-9_]* but it cannot be changed
  // because then some tests in partest don't pass
  private def mayBeValidPackage(dirName: String): Boolean =
    (dirName != "META-INF") && (dirName != "") && (dirName.charAt(0) != '.')
}
