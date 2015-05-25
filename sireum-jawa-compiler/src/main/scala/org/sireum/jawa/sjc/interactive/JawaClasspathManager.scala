/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.interactive

import org.sireum.util._
import java.io.InputStream
import org.sireum.jawa.sjc.Signature
import org.sireum.jawa.sjc.ResolveLevel
import org.sireum.jawa.sjc.util.MyFileUtil
import org.sireum.jawa.sjc.util.LibraryAPISummary
import org.sireum.jawa.sjc.ObjectType
import org.sireum.jawa.sjc.JavaKnowledge
import org.sireum.jawa.sjc.io.AbstractFile
import org.sireum.jawa.sjc.io.Directory
import org.sireum.jawa.sjc.io.PlainDirectory

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait JawaClasspathManager extends JavaKnowledge with ResolveLevel {
  
  private final val TITLE = "JawaClasspathManager"
  
  object ClassCategory extends Enumeration {
    val APPLICATION, THIRD_PARTY_LIBRARY, SYSTEM_LIBRARY = Value
  }
  
  /**
   * force set preload
   */
  def setPreLoadFlag = this.preLoaded = true
  
  /**
   * pre-load all the code of the library
   */
  def preLoad(fileRoot: PlainDirectory, ext: String) = {
    val filesIt = fileRoot.iterator
    filesIt.foreach{
      file => 
        val recName = getClassName(file.text)
        val recType = getTypeFromName(recName).asInstanceOf[ObjectType]
        setClassFile(recType, file, Left(ClassCategory.SYSTEM_LIBRARY))
    }
    this.preLoaded = true
  }
  
  /**
   * load code from given root dir
   */
  def load(fileRoot: PlainDirectory, ext: String, summary: LibraryAPISummary) = {
    val filesIt = fileRoot.iterator
    filesIt.foreach{
      file => 
        val recName = getClassName(file.text)
        val recType = getTypeFromName(recName).asInstanceOf[ObjectType]
        setClassFile(recType, file, Right(summary))
    }
  }
  
  private def getClassName(rCode: String): String = {
    if(getFirstWord(rCode) != "record") throw new RuntimeException("Following code has format problem: \n" + rCode)
    val size = rCode.size
    var i = rCode.indexOf("record") + 7
    while (i < size && (rCode.charAt(i).isWhitespace || rCode.charAt(i) == '`')) {
      i += 1
    }
    var j = i
    while (j < size && !(rCode.charAt(j) == '`') && !rCode.charAt(j).equals("@")) {
      j += 1
    }
    if (i < size && j <= size) rCode.substring(i, j)
    else throw new RuntimeException("Doing " + TITLE + ". Cannot find name from record code: " + rCode)
  }
  
  private def getFirstWord(line: String) = {
    val size = line.size
    var i = 0
    while (i < size && line.charAt(i).isWhitespace) {
      i += 1
    }
    var j = i
    while (j < size && !line.charAt(j).isWhitespace) {
      j += 1
    }
    if (i < size && j <= size) line.substring(i, j)
    else ""
  }
  
  /**
   * did preLoad happen or not?
   */
  protected var preLoaded = false
  
  /**
   * did preLoad happen or not?
   */
  def isPreLoaded = this.preLoaded
  
  /**
   * map from class name to pilar code of library. E.g. record name java.lang.Object to its pilar code 
   */
  protected val systemLibraryClassFiles: MMap[ObjectType, AbstractFile] = mmapEmpty
	
	/**
   * map from class name to pilar code of library. E.g. record name java.lang.Object to its pilar code 
   */
	protected val thirdPartyLibraryClassFiles: MMap[ObjectType, AbstractFile] = mmapEmpty
	
	/**
   * map from class name to pilar code of app. E.g. record name java.lang.MyObject to its pilar code 
   */
	protected val applicationClassFiles: MMap[ObjectType, AbstractFile] = mmapEmpty
	
	/**
	 * set lib class code
	 */
	def addSystemLibraryClassFile(typ: ObjectType, file: AbstractFile) = this.systemLibraryClassFiles(typ) = file
	
	/**
	 * set app using lib class code
	 */
	def addThirdPartyLibraryClassFile(typ: ObjectType, file: AbstractFile) = this.thirdPartyLibraryClassFiles(typ) = file
	
	/**
	 * clear app class codes
	 */
	def clearAppClassFiles = {
    this.applicationClassFiles.clear()
    this.thirdPartyLibraryClassFiles.clear()
  }
	
	/**
	 * set app record code
	 */
	def addApplicationClassFile(typ: ObjectType, file: AbstractFile) = this.applicationClassFiles(typ) = file
	
	/**
	 * set record code
	 */
	def setClassFile(typ: ObjectType, file: AbstractFile, determiner: Either[ClassCategory.Value, LibraryAPISummary]) = {
    determiner match{
      case Left(codetyp) =>
        codetyp match{
          case ClassCategory.SYSTEM_LIBRARY => addSystemLibraryClassFile(typ, file)
          case ClassCategory.APPLICATION => addApplicationClassFile(typ, file)
          case ClassCategory.THIRD_PARTY_LIBRARY => addThirdPartyLibraryClassFile(typ, file)
        }
      case Right(summary) =>
        summary.isLibraryAPI(typ) match{
		      case true => addThirdPartyLibraryClassFile(typ, file)
		      case false => addApplicationClassFile(typ, file)
		    }
    }
  }
	
	/**
	 * get record code
	 */
	def getClassFile(typ: ObjectType): AbstractFile = {
    val file = this.applicationClassFiles.get(typ) match{
      case Some(file) => file
      case None =>
        this.thirdPartyLibraryClassFiles.get(typ) match{
          case Some(file) => file
          case None =>
            this.systemLibraryClassFiles.getOrElse(typ, throw new RuntimeException("Class " + typ + " does not exist in the current code base."))
        }
    }
    file
  }
	
	/**
	 * return record codes type
	 */
	def getFileType(typ: ObjectType): ClassCategory.Value = {
	  if(this.applicationClassFiles.contains(typ)) ClassCategory.APPLICATION
	  else if(this.thirdPartyLibraryClassFiles.contains(typ)) ClassCategory.THIRD_PARTY_LIBRARY
	  else if(this.systemLibraryClassFiles.contains(typ)) ClassCategory.SYSTEM_LIBRARY
	  else throw new RuntimeException("record " + typ + " does not exist in the current code base.")
	}
	
	/**
	 * contains given record or not?
	 */
	def containsClassFile(typ: ObjectType): Boolean = this.applicationClassFiles.contains(typ) || this.thirdPartyLibraryClassFiles.contains(typ) || this.systemLibraryClassFiles.contains(typ)
	
	/**
	 * print all content
	 */
	def printContent = {
	  println("appClassesFiles:")
	  this.applicationClassFiles.foreach{
	    case (k, v)=>
	      println("recName: " + k)
	      println(v)
	  }
	}
}