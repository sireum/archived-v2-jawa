package org.sireum.jawa

import org.sireum.util.FileResourceUri
import org.sireum.util.FileUtil
import org.sireum.jawa.pilarParser.LightWeightPilarParser
import org.sireum.jawa.util.StringFormConverter
import org.sireum.util.ISet
import java.io.InputStream

object JawaCodeSource {
  
  object CodeType extends Enumeration {
    val APP, APP_USING_LIBRARY, ANDROID_LIBRARY = Value
  }
  
  /**
   * pre-load all the code of the library
   */
  
  def preLoad(fileResourceUris : ISet[FileResourceUri]) = {
    fileResourceUris.foreach{
      fUri =>
        LightWeightPilarParser(Right(fUri), Left(CodeType.ANDROID_LIBRARY))
    }
    this.preLoaded = true
  }
  
  /**
   * load code from given file resource
   */
  
  def load(fileUri : FileResourceUri, summary : LibraryAPISummary) = {
    LightWeightPilarParser(Right(fileUri), Right(summary))
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
   * map from record name to pilar code of library. E.g. record name [|java:lang:Object|] to its pilar code 
   */
  
	protected var libRecordsCodes : Map[String, String] = Map()
	
	 /**
   * map from record name to pilar code of library. E.g. record name [|java:lang:Object|] to its pilar code 
   */
  
	protected var appUsingLibRecordsCodes : Map[String, String] = Map()
	
	/**
   * map from record name to pilar code of app. E.g. record name [|java:lang:MyObject|] to its pilar code 
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
          case CodeType.ANDROID_LIBRARY => setLibraryRecordCode(name, code)
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
	  else if(this.libRecordsCodes.contains(name)) CodeType.ANDROID_LIBRARY
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