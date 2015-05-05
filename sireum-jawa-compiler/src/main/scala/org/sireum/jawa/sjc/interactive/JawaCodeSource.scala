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
import org.sireum.jawa.sjc.util.LightWeightPilarParser
import org.sireum.jawa.sjc.util.MyFileUtil
import org.sireum.jawa.sjc.util.LibraryAPISummary
import org.sireum.jawa.sjc.ObjectType
import org.sireum.jawa.sjc.JavaKnowledge

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object JawaCodeSource extends JavaKnowledge with ResolveLevel {
  
  final val TITLE = "JawaCodeSource"
  
  object CodeType extends Enumeration {
    val APP, THIRD_PARTY_LIBRARY, SYSTEM_LIBRARY = Value
  }
  
  /**
   * pre-load all the code of the library
   */
  def preLoad(fileResourceUris: ISet[FileResourceUri]) = {
    fileResourceUris.foreach{
      fUri =>
        LightWeightPilarParser(fUri, Left(CodeType.SYSTEM_LIBRARY))
    }
    this.preLoaded = true
  }
  
  /**
   * force set preload
   */
  def setPreLoadFlag = this.preLoaded = true
  
  /**
   * load code from given file resource
   */
  def load(fileUri: FileResourceUri, summary: LibraryAPISummary) = {
    LightWeightPilarParser(fileUri, Right(summary))
  }
  
  /**
   * pre-load all the code of the library
   */
  def preLoad(fileRootUri: FileResourceUri, ext: String) = {
    val fileUris = FileUtil.listFiles(fileRootUri, ext, true)
    fileUris.foreach{
      fileUri => 
        val recCode = MyFileUtil.readFileContent(fileUri)
        val recName = getClassName(recCode)
        val recType = getTypeFromName(recName).asInstanceOf[ObjectType]
        setClassCode(recType, fileUri, recCode, Left(CodeType.SYSTEM_LIBRARY))
    }
    this.preLoaded = true
  }
  
  /**
   * load code from given root dir
   */
  
  def load(fileRootUri: FileResourceUri, ext: String, summary: LibraryAPISummary) = {
    val fileUris = FileUtil.listFiles(fileRootUri, ext, true)
    fileUris.foreach{
      fileUri => 
        val recCode = MyFileUtil.readFileContent(fileUri)
        val recName = getClassName(recCode)
        val recType = getTypeFromName(recName).asInstanceOf[ObjectType]
        setClassCode(recType, fileUri, recCode, Right(summary))
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
  protected val systemLibraryClassCodes: MMap[ObjectType, (FileResourceUri, String)] = mmapEmpty
	
	/**
   * map from class name to pilar code of library. E.g. record name java.lang.Object to its pilar code 
   */
	protected val thirdPartyLibraryClassCodes: MMap[ObjectType, (FileResourceUri, String)] = mmapEmpty
	
	/**
   * map from class name to pilar code of app. E.g. record name java.lang.MyObject to its pilar code 
   */
	protected val applicationClassCodes: MMap[ObjectType, (FileResourceUri, String)] = mmapEmpty
	
	/**
	 * get lib class codes
	 */
	def getSystemLibraryCodes: ISet[(FileResourceUri, String)] = this.systemLibraryClassCodes.values.toSet
	
	/**
	 * set lib class code
	 */
	def addSystemLibraryClassCode(typ: ObjectType, fileUri: FileResourceUri, code: String) = this.systemLibraryClassCodes(typ) = (fileUri, code)
	
	/**
	 * get app using lib class codes
	 */
	def getThirdPartyLibraryClassCodes: ISet[(FileResourceUri, String)] = this.thirdPartyLibraryClassCodes.values.toSet
	
	/**
	 * set app using lib class code
	 */
	def addThirdPartyLibraryClassCode(typ: ObjectType, fileUri: FileResourceUri, code: String) = this.thirdPartyLibraryClassCodes(typ) = (fileUri, code)
	
	/**
	 * get app class codes
	 */
	
	def getApplicationClassCodes: ISet[(FileResourceUri, String)] = this.applicationClassCodes.values.toSet
	
	/**
	 * clear app class codes
	 */
	def clearAppClassCodes = {
    this.applicationClassCodes.clear()
    this.thirdPartyLibraryClassCodes.clear()
  }
	
	/**
	 * set app record code
	 */
	def addApplicationClassCode(typ: ObjectType, fileUri: FileResourceUri, code: String) = this.applicationClassCodes(typ) = (fileUri, code)
	
	/**
	 * set record code
	 */
	def setClassCode(typ: ObjectType, fileUri: FileResourceUri, code: String, determiner: Either[CodeType.Value, LibraryAPISummary]) = {
    determiner match{
      case Left(codetyp) =>
        codetyp match{
          case CodeType.SYSTEM_LIBRARY => addSystemLibraryClassCode(typ, fileUri, code)
          case CodeType.APP => addApplicationClassCode(typ, fileUri, code)
          case CodeType.THIRD_PARTY_LIBRARY => addThirdPartyLibraryClassCode(typ, fileUri, code)
        }
      case Right(summary) =>
        summary.isLibraryAPI(typ) match{
		      case true => addThirdPartyLibraryClassCode(typ, fileUri, code)
		      case false => addApplicationClassCode(typ, fileUri, code)
		    }
    }
  }
	
	/**
	 * get record code
	 */
	def getClassCode(typ: ObjectType, level: ResolveLevel.Value): (FileResourceUri, String) = {
    var code = this.applicationClassCodes.get(typ) match{
      case Some(code) => code
      case None =>
        this.thirdPartyLibraryClassCodes.get(typ) match{
          case Some(code) => code
          case None =>
            this.systemLibraryClassCodes.getOrElse(typ, throw new RuntimeException("Class " + typ + " does not exist in the current code base."))
        }
    }
//    if(level < ResolveLevel.BODY){
//      code = LightWeightPilarParser.getEmptyBodyCode(code._2)
//    }
    code
  }
	
	/**
	 * return record codes type
	 */
	def getCodeType(typ: ObjectType): CodeType.Value = {
	  if(this.applicationClassCodes.contains(typ)) CodeType.APP
	  else if(this.thirdPartyLibraryClassCodes.contains(typ)) CodeType.THIRD_PARTY_LIBRARY
	  else if(this.systemLibraryClassCodes.contains(typ)) CodeType.SYSTEM_LIBRARY
	  else throw new RuntimeException("record " + typ + " does not exist in the current code base.")
	}
	
	/**
	 * contains given record or not?
	 */
	def containsClass(typ: ObjectType): Boolean = this.applicationClassCodes.contains(typ) || this.thirdPartyLibraryClassCodes.contains(typ) || this.systemLibraryClassCodes.contains(typ)
	
	/**
	 * contains given procedure's container record or not?
	 */
	
	def containsMethod(sig: Signature): Boolean = {
    getMethodCode(sig).isDefined
  }
	
	/**
	 * get procedure's containing record's code
	 */
	def getMethodCode(sig: Signature): Option[String] = {
    val typ = sig.getClassType
    val (f, recordCode) = getClassCode(typ, ResolveLevel.BODY)
    LightWeightPilarParser.getCode(recordCode, sig)
  }

	def getMethodCodeWithoutFailing(sig: Signature): String = {
    getMethodCode(sig) match{
      case Some(code) => code
      case None => throw new RuntimeException("Given proc sig " + sig + " does not exisit.")
    }
  }
	
	/**
	 * print all content
	 */
	def printContent = {
	  println("appClassesCodes:")
	  this.applicationClassCodes.foreach{
	    case (k, v)=>
	      println("recName: " + k)
	      println(v)
	  }
	}
}