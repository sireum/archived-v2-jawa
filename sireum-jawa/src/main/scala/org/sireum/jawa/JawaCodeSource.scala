/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa

import org.sireum.util.FileResourceUri
import org.sireum.util.FileUtil
import org.sireum.jawa.util.StringFormConverter
import org.sireum.util.ISet
import java.io.InputStream
import org.sireum.jawa.util.MyFileUtil

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object JawaCodeSource {
  
  final val TITLE = "JawaCodeSource"
  
  object CodeType extends Enumeration {
    val APP, THIRD_PARTY_LIB, FRAMEWORK = Value
  }
  
  /**
   * pre-load all the code of the library
   */
  
  def preLoad(fileResourceUris : ISet[FileResourceUri]) = {
    fileResourceUris.foreach{
      fUri =>
        LightWeightPilarParser(Right(fUri), Left(CodeType.FRAMEWORK))
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
  
  def load(fileUri : FileResourceUri, summary : LibraryAPISummary) = {
    LightWeightPilarParser(Right(fileUri), Right(summary))
  }
  
  /**
   * pre-load all the code of the library
   */
  
  def preLoad(fileRootUri : FileResourceUri, ext : String) = {
    val fileUris = FileUtil.listFiles(fileRootUri, ext, true)
    fileUris.foreach{
      fileUri => 
        val recCode = MyFileUtil.readFileContent(fileUri)
        val recName = getClassName(recCode)
        setClassCode(recName, recCode, Left(CodeType.FRAMEWORK))
    }
    this.preLoaded = true
  }
  
  /**
   * load code from given root dir
   */
  
  def load(fileRootUri : FileResourceUri, ext : String, summary : LibraryAPISummary) = {
    val fileUris = FileUtil.listFiles(fileRootUri, ext, true)
    fileUris.foreach{
      fileUri => 
        val recCode = MyFileUtil.readFileContent(fileUri)
        val recName = getClassName(recCode)
        setClassCode(recName, recCode, Right(summary))
    }
  }
  
  private def getClassName(rCode : String) : String = {
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
  
  private def getFirstWord(line : String) = {
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
  
  protected var frameworkClassCodes : Map[String, String] = Map()
	
	/**
   * map from class name to pilar code of library. E.g. record name java.lang.Object to its pilar code 
   */
  
	protected var thirdPartyLibClassCodes : Map[String, String] = Map()
	
	/**
   * map from class name to pilar code of app. E.g. record name java.lang.MyObject to its pilar code 
   */
  
	protected var appClassCodes : Map[String, String] = Map()
	
//	/**
//	 * map from procedure sig to container record name. sig e.g. [|Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z|]
//	 */
//	
//	protected var proceduresCodes : Map[String, String] = Map()
//	
//	/**
//	 * map from global variable full-qualified name to container record name. e.g. @@[|java:lang:Enum.serialVersionUID|]
//	 */
//	
//	protected var globalVarsCodes : Map[String, String] = Map()
	
	/**
	 * get lib class codes
	 */
	
	def getFrameworkClassCodes = this.frameworkClassCodes
	
	/**
	 * set lib class code
	 */
	
	def setFrameworkClassCode(name : String, code : String) = this.frameworkClassCodes += (name -> code)
	
	/**
	 * get app using lib class codes
	 */
	
	def getThirdPartyLibraryClassCodes = this.thirdPartyLibClassCodes
	
	/**
	 * set app using lib class code
	 */
	
	def setThirdPartyLibraryClassCode(name : String, code : String) = this.thirdPartyLibClassCodes += (name -> code)
	
	/**
	 * get app class codes
	 */
	
	def getAppClassCodes = this.appClassCodes
	
	/**
	 * clear app class codes
	 */
	
	def clearAppClassCodes = {
    this.appClassCodes = Map()
    this.thirdPartyLibClassCodes = Map()
  }
	
	/**
	 * set app record code
	 */
	
	def setAppClassCode(name : String, code : String) = this.appClassCodes += (name -> code)
	
	/**
	 * set record code
	 */
	
	def setClassCode(name : String, code : String, determiner : Either[CodeType.Value, LibraryAPISummary]) = {
    determiner match{
      case Left(typ) =>
        typ match{
          case CodeType.FRAMEWORK => setFrameworkClassCode(name, code)
          case CodeType.APP => setAppClassCode(name, code)
          case CodeType.THIRD_PARTY_LIB => setThirdPartyLibraryClassCode(name, code)
        }
      case Right(summary) =>
        summary.isLibraryAPI(name) match{
		      case true => setThirdPartyLibraryClassCode(name, code)
		      case false => setAppClassCode(name, code)
		    }
    }
  }
	
	def addAppClassCode(name : String, procCode : String) = {
	  val recCode = this.appClassCodes.getOrElse(name, throw new RuntimeException("Class " + name + " does not exist in the app code."))
	  val newRecCode = recCode + "\n" + procCode
	  setAppClassCode(name, newRecCode)
	}
	
	/**
	 * get record code
	 */
	
	def getClassCode(name : String, level : Center.ResolveLevel.Value) : String = {
    var code = this.appClassCodes.get(name) match{
      case Some(code) => code
      case None =>
        this.thirdPartyLibClassCodes.get(name) match{
          case Some(code) => code
          case None =>
            this.frameworkClassCodes.getOrElse(name, throw new RuntimeException("Class " + name + " does not exist in the current code base."))
        }
    }
    if(level < Center.ResolveLevel.BODY){
      code = LightWeightPilarParser.getEmptyBodyCode(code)
    }
    code
  }
	
	/**
	 * return record codes type
	 */
	
	def getCodeType(name : String) : CodeType.Value = {
	  if(this.appClassCodes.contains(name)) CodeType.APP
	  else if(this.thirdPartyLibClassCodes.contains(name)) CodeType.THIRD_PARTY_LIB
	  else if(this.frameworkClassCodes.contains(name)) CodeType.FRAMEWORK
	  else throw new RuntimeException("record " + name + " does not exist in the current code base.")
	}
	
	/**
	 * contains given record or not?
	 */
	
	def containsClass(name : String) : Boolean = this.appClassCodes.contains(name) || this.thirdPartyLibClassCodes.contains(name) || this.frameworkClassCodes.contains(name)
	
	/**
	 * contains given procedure's container record or not?
	 */
	
	def containsMethod(sig : String) : Boolean = {
    getMethodCode(sig).isDefined
  }
	
	/**
	 * contains given global var's container record or not?
	 */
	
	def containsGlobalVar(sig : String) : Boolean = {
	  getMethodCode(sig).isDefined
	}
//	/**
//	 * set procedure container name
//	 */
//	
//	def setMethodContainer(sig : String, recName : String) = this.proceduresCodes += (sig -> recName)
//	
	/**
	 * get procedure's containing record's code
	 */
	
	def getMethodCode(sig : String) : Option[String] = {
    val name = StringFormConverter.getClassNameFromMethodSignature(sig)
    val recordCode = getClassCode(name, Center.ResolveLevel.BODY)
    LightWeightPilarParser.getCode(recordCode, sig)
  }

	def getMethodCodeWithoutFailing(sig : String) : String = {
    getMethodCode(sig) match{
      case Some(code) => code
      case None => throw new RuntimeException("Given proc sig " + sig + " does not exisit.")
    }
  }
	
	/**
	 * get global variable's containing record's code.
	 */
	
	def getGlobalVarCode(sig : String) : Option[String] = {
	  val name = StringFormConverter.getClassNameFromFieldSignature(sig)
    val classCode = getClassCode(name, Center.ResolveLevel.BODY)
    LightWeightPilarParser.getCode(classCode, sig)
	}
	
	def getGlobalVarCodeWithoutFailing(sig : String) : String = {
    getGlobalVarCode(sig) match{
      case Some(code) => code
      case None => throw new RuntimeException("Given global var sig " + sig + " does not exisit.")
    }
  }
	
	/**
	 * print all content
	 */
	
	def printContent = {
	  println("appClassesCodes:")
	  this.appClassCodes.foreach{
	    case (k, v)=>
	      println("recName: " + k)
	      println(v)
	  }
	}
}