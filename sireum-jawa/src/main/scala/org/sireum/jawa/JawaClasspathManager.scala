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
import org.sireum.jawa.classpath.ClassRepresentation
import org.sireum.jawa.util.MyFileUtil
import org.sireum.jawa.io.FgSourceFile
import org.sireum.jawa.io.PlainFile
import org.sireum.jawa.io.SourceFile
import org.sireum.jawa.sourcefile.SourcefileParser
import org.sireum.jawa.classfile.ClassfileParser

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait JawaClasspathManager extends JavaKnowledge { self: Global =>
  
  private final val TITLE = "JawaClasspathManager"
  
  /**
   * load code from given root dir
   */
  def load(fileRootUri: FileResourceUri, ext: String, summary: LibraryAPISummary) = {
    val fileUris = FileUtil.listFiles(fileRootUri, ext, true)
    fileUris.foreach{
      fileUri =>
        val source = new FgSourceFile(new PlainFile(FileUtil.toFile(fileUri)))
        val codes = source.getClassCodes
        val classTypes: MSet[JawaType] = msetEmpty
        codes.foreach{
          code =>
            try {
              val className = LightWeightPilarParser.getClassName(code)
              classTypes += JavaKnowledge.getTypeFromName(className)
            } catch {
              case e: Exception => reporter.warning(TITLE, e.getMessage)
            }
        }
        classTypes.foreach {
          typ =>
            summary.isLibraryClass(typ) match {
              case true => this.userLibraryClassCodes(typ) = source
              case false => this.applicationClassCodes(typ) = source
            }
        }
    }
  }
  
  def getClassCategoryFromClassPath(typ: JawaType): ClassCategory.Value = {
    this.applicationClassCodes.contains(typ) match {
      case true => ClassCategory.APPLICATION
      case false =>
        this.userLibraryClassCodes.contains(typ) match {
          case true => ClassCategory.USER_LIBRARY
          case false =>
            ClassCategory.SYSTEM_LIBRARY
        }
    }
  }
  
  /**
   * map from class name to pilar code of library. E.g. class type java.lang.Object to its file
   */
  protected val userLibraryClassCodes: MMap[JawaType, SourceFile] = mmapEmpty
  
  /**
   * map from class name to pilar code of app. E.g. record name java.lang.MyObject to its file
   */
  protected val applicationClassCodes: MMap[JawaType, SourceFile] = mmapEmpty
  
  def getUserLibraryClassCodes: IMap[JawaType, SourceFile] = this.userLibraryClassCodes.toMap
  
  def getApplicationClassCodes: IMap[JawaType, SourceFile] = this.applicationClassCodes.toMap
  
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
  
  protected val cachedClassRepresentation: MMap[JawaType, ClassRepresentation] = mmapEmpty
  
  def containsClassFile(typ: JawaType): Boolean = {
    this.applicationClassCodes.contains(typ) ||
    this.userLibraryClassCodes.contains(typ) ||
    cachedClassRepresentation.contains(typ)  ||
    {
      classPath.findClass(typ.name) match {
        case Some(c) =>
          cachedClassRepresentation(typ) = c
          true
        case None =>
          false
      }
    }
  }
  
  /**
   * get procedure's containing record's code
   */
  def getMethodCode(sig: Signature) : Option[String] = {
    val typ = sig.getClassType
    getMyClass(typ) match {
      case Some(mc) =>
        this.applicationClassCodes.get(typ) match {
          case Some(asrc) =>
            val recordCode = getClassCode(asrc.file, ResolveLevel.BODY)
            LightWeightPilarParser.getCode(recordCode, sig.signature)
          case None =>
            this.userLibraryClassCodes.get(typ) match {
              case Some(usrc) =>
                val recordCode = getClassCode(usrc.file, ResolveLevel.BODY)
                LightWeightPilarParser.getCode(recordCode, sig.signature)
              case None =>
                None
            }
        }
      case _ => None
    }
  }
  
  def getMyClass(typ: JawaType): Option[MyClass] = {
    this.applicationClassCodes.get(typ) match {
      case Some(asrc) =>
        SourcefileParser.parse(asrc, ResolveLevel.HIERARCHY, reporter).get(typ)
      case None =>
        this.userLibraryClassCodes.get(typ) match {
          case Some(usrc) =>
            SourcefileParser.parse(usrc, ResolveLevel.HIERARCHY, reporter).get(typ)
          case None =>
            cachedClassRepresentation.get(typ) match {
              case Some(cs) =>
                ClassfileParser.parse(cs.binary.get).get(typ)
              case None =>
                classPath.findClass(typ.name) match {
                  case Some(c) =>
                    cachedClassRepresentation(typ) = c
                    ClassfileParser.parse(c.binary.get).get(typ)
                  case None =>
                    None
                }
            }
        }
    }
  }
  
  def getClassRepresentation(typ: JawaType): Option[ClassRepresentation] = {
    this.cachedClassRepresentation.get(typ) match {
      case None =>
        classPath.findClass(typ.name) match {
          case Some(c) =>
            cachedClassRepresentation(typ) = c
            Some(c)
          case None =>
            None
        }
      case a => a
    }
  }
  
  protected[jawa] def processClassRepresentation(cr: ClassRepresentation) = {
    cr.source match {
      case Some(f) =>
        println(f.text)
      case None =>
    }
  }
}