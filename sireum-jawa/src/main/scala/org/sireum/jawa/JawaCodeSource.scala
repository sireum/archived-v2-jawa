package org.sireum.jawa

import org.sireum.util.FileResourceUri
import org.sireum.util.FileUtil
import org.sireum.jawa.pilarParser.LightWeightPilarParser
import org.sireum.jawa.util.StringFormConverter
import org.sireum.util.ISet
import java.io.InputStream
import org.sireum.jawa.util.MyFileUtil

object JawaCodeSource {
  
  final val TITLE = "JawaCodeSource"
  
  object CodeType extends Enumeration {
    val APP, APP_USING_LIBRARY, LIBRARY = Value
  }
  
  /**
   * pre-load all the code of the library
   */
  
  def preLoad(fileResourceUris : ISet[FileResourceUri]) = {
    fileResourceUris.foreach{
      fUri =>
        LightWeightPilarParser(Right(fUri), Left(CodeType.LIBRARY))
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
        val recName = getRecordName(recCode)
        setRecordCode(recName, recCode, Left(CodeType.LIBRARY))
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
        val recName = getRecordName(recCode)
        setRecordCode(recName, recCode, Right(summary))
    }
  }
  
  private def getRecordName(rCode : String) : String = {
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
   * map from record name to pilar code of library. E.g. record name java.lang.Object to its pilar code 
   */
  
	protected var libRecordsCodes : Map[String, String] = Map()
	
	 /**
   * map from record name to pilar code of library. E.g. record name java.lang.Object to its pilar code 
   */
  
	protected var appUsingLibRecordsCodes : Map[String, String] = Map()
	
	/**
   * map from record name to pilar code of app. E.g. record name java.lang.MyObject to its pilar code 
   */
  
	protected var appRecordsCodes : Map[String, String] = Map()
	
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
	 * get lib records' code
	 */
	
	def getLibraryRecordsCodes = this.libRecordsCodes
	
	/**
	 * set lib record code
	 */
	
	def setLibraryRecordCode(name : String, code : String) = this.libRecordsCodes += (name -> code)
	
	/**
	 * get app using lib records' code
	 */
	
	def getAppUsingLibraryRecordsCodes = this.appUsingLibRecordsCodes
	
	/**
	 * set app using lib record code
	 */
	
	def setAppUsingLibraryRecordCode(name : String, code : String) = this.appUsingLibRecordsCodes += (name -> code)
	
	/**
	 * get app records codes
	 */
	
	def getAppRecordsCodes = this.appRecordsCodes
	
	/**
	 * clear app records codes
	 */
	
	def clearAppRecordsCodes = {
    this.appRecordsCodes = Map()
    this.appUsingLibRecordsCodes = Map()
  }
	
	/**
	 * set app record code
	 */
	
	def setAppRecordCode(name : String, code : String) = this.appRecordsCodes += (name -> code)
	
	/**
	 * set record code
	 */
	
	def setRecordCode(name : String, code : String, determiner : Either[CodeType.Value, LibraryAPISummary]) = {
    determiner match{
      case Left(typ) =>
        typ match{
          case CodeType.LIBRARY => setLibraryRecordCode(name, code)
          case CodeType.APP => setAppRecordCode(name, code)
          case CodeType.APP_USING_LIBRARY => setAppUsingLibraryRecordCode(name, code)
        }
      case Right(summary) =>
        summary.isLibraryAPI(name) match{
		      case true => setAppUsingLibraryRecordCode(name, code)
		      case false => setAppRecordCode(name, code)
		    }
    }
  }
	
	def addAppRecordCode(name : String, procCode : String) = {
	  val recCode = this.appRecordsCodes.getOrElse(name, throw new RuntimeException("Record " + name + " does not exist in the app code."))
	  val newRecCode = recCode + "\n" + procCode
	  setAppRecordCode(name, newRecCode)
	}
	
	/**
	 * get record code
	 */
	
	def getRecordCode(name : String, level : Center.ResolveLevel.Value) : String = {
    var code = this.appRecordsCodes.get(name) match{
      case Some(code) => code
      case None =>
        this.appUsingLibRecordsCodes.get(name) match{
          case Some(code) => code
          case None =>
            this.libRecordsCodes.getOrElse(name, throw new RuntimeException("record " + name + " does not exist in the current code base."))
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
	  if(this.appRecordsCodes.contains(name)) CodeType.APP
	  else if(this.appUsingLibRecordsCodes.contains(name)) CodeType.APP_USING_LIBRARY
	  else if(this.libRecordsCodes.contains(name)) CodeType.LIBRARY
	  else throw new RuntimeException("record " + name + " does not exist in the current code base.")
	}
	
	/**
	 * contains given record or not?
	 */
	
	def containsRecord(name : String) : Boolean = this.appRecordsCodes.contains(name) || this.appUsingLibRecordsCodes.contains(name) || this.libRecordsCodes.contains(name)
	
	/**
	 * contains given procedure's container record or not?
	 */
	
	def containsProcedure(sig : String) : Boolean = {
    getProcedureCode(sig).isDefined
  }
	
	/**
	 * contains given global var's container record or not?
	 */
	
	def containsGlobalVar(sig : String) : Boolean = {
	  getProcedureCode(sig).isDefined
	}
//	/**
//	 * set procedure container name
//	 */
//	
//	def setProcedureContainer(sig : String, recName : String) = this.proceduresCodes += (sig -> recName)
//	
	/**
	 * get procedure's containing record's code
	 */
	
	def getProcedureCode(sig : String) : Option[String] = {
    val name = StringFormConverter.getRecordNameFromProcedureSignature(sig)
    val recordCode = getRecordCode(name, Center.ResolveLevel.BODY)
    LightWeightPilarParser.getCode(recordCode, sig)
  }

	def getProcedureCodeWithoutFailing(sig : String) : String = {
    getProcedureCode(sig) match{
      case Some(code) => code
      case None => throw new RuntimeException("Given proc sig " + sig + " does not exisit.")
    }
  }
	
	/**
	 * get global variable's containing record's code.
	 */
	
	def getGlobalVarCode(sig : String) : Option[String] = {
	  val name = StringFormConverter.getRecordNameFromFieldSignature(sig)
    val recordCode = getRecordCode(name, Center.ResolveLevel.BODY)
    LightWeightPilarParser.getCode(recordCode, sig)
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
	  println("codes:")
	  this.libRecordsCodes.foreach{
	    case (k, v)=>
	      println("recName: " + k)
	      println(v)
	  }
	}
}