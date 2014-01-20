package org.sireum.jawa.pilarParser

import java.net._
import java.io._
import org.sireum.util._
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.LibraryAPISummary

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object LightWeightPilarParser {

  val TITLE = "LightWeightPilarParser"
    
  val DEBUG = false
  
  def apply(source : Either[String, FileResourceUri], determiner : Either[JawaCodeSource.CodeType.Value, LibraryAPISummary]) =
    parseModel(source, determiner)

  def parseModel(source : Either[String, FileResourceUri], determiner : Either[JawaCodeSource.CodeType.Value, LibraryAPISummary]) = {
    source match {
      case Left(code) =>
        readModelChunks(new StringReader(code), determiner)
      case Right(fileResourceUri) =>
        val fr = new FileReader(new File(new URI(fileResourceUri)))
        try readModelChunks(fr, determiner) finally fr.close
    }
    if(DEBUG)
    	JawaCodeSource.printContent
  }
  
  def getEmptyBodyCode(recordCode : String) : String = {
    val emptyBody = 
"""
# return;
  }      
"""
    val lnr = new LineNumberReader(new StringReader(recordCode))
    var lineNo = 0
    var chunkLineNo = 0
    val sb = new StringBuilder
    var lineText = lnr.readLine
    val keyword = "procedure"
    var copy = true
    while (lineText != null) {
      val word = getFirstWord(lineText)
      if (keyword == word) {
        copy = false
        chunkLineNo = lineNo
        sb.append(lineText)
	      sb.append('\n')
        sb.append(emptyBody)
	      sb.append('\n')
      }
			if(copy){
        sb.append(lineText)
	      sb.append('\n')
      }

      lineNo += 1

      lineText = lnr.readLine
    }
    sb.toString.intern()
  }
  
  def getCode(recordCode : String, contentSig : String) : Option[String] = {
    val lnr = new LineNumberReader(new StringReader(recordCode))
    var lineNo = 0

    var chunkLineNo = 0
    val sb = new StringBuilder

    var lineText = lnr.readLine

    val keywords = Set("package", "const", "enum", "typealias", "record",
      "global", "procedure", "vset", "fun", "extension")

    var found = false
    import scala.util.control.Breaks._
    breakable{
	    while (lineText != null) {
	      val word = getFirstWord(lineText)
	      if (keywords.contains(word) && found == true) break
	      if (keywords.contains(word)) {
	        if(lineText.contains(contentSig))
	        	found = true
	        	
	        chunkLineNo = lineNo
	      }
	
	      if(found){
		      sb.append(lineText)
		      sb.append('\n')
	      }
	      lineNo += 1
	
	      lineText = lnr.readLine
	    }
    }
    if(found) Some(sb.toString.intern())
    else None
  }
  
  /**
   * read the code of each "record" and stores it in AmandroidCodeSource as a map: (recordName -> code). 
   * Note that each record block in pilar source starts with keyword "record"; 
   * so, the searching starts with keyword "record" and goes on until find another "record". The procs inside a record x are included in the code of x.
   */
  def readModelChunks(r : Reader, determiner : Either[JawaCodeSource.CodeType.Value, LibraryAPISummary]) = {
    val lnr = new LineNumberReader(r)
    var lineNo = 0

    var chunkLineNo = 0
    var sb = new StringBuilder

    var lineText = lnr.readLine

    val keyword = "record"

    var recName : String = null
    while (lineText != null) {
      val word = getFirstWord(lineText)

      if (keyword == word) {
        if(recName!=null)
        	JawaCodeSource.setRecordCode(recName, sb.toString, determiner)
        recName = getRecordName(lineText)

        sb = new StringBuilder
        chunkLineNo = lineNo
      }

      sb.append(lineText)
      sb.append('\n')
      lineNo += 1

      lineText = lnr.readLine
    }
    JawaCodeSource.setRecordCode(recName, sb.toString, determiner)
  }

  def getFirstWord(line : String) = {
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
  
  def getRecordName(line : String) : String = {
    val size = line.size
    var i = if(line.contains("record")) (line.indexOf("record") + 7) else size
    while (i < size && line.charAt(i).isWhitespace) {
      i += 1
    }
    var j = i
    while (j < size && !line.charAt(j).isWhitespace && !line.charAt(j).equals("@")) {
      j += 1
    }
    if (i < size && j <= size) line.substring(i, j)
    else throw new RuntimeException("Doing " + TITLE + ". Cannot find name from record code: " + line)
  }
  
//  def getProcedureSignature(line : String) : String = {
//    val size = line.size
//    var i = if(line.contains("@signature")) (line.indexOf("@signature") + 10) else size
//    while (i < size && line.charAt(i).isWhitespace) {
//      i += 1
//    }
//    var j = i
//    while (j < size && !line.charAt(j).isWhitespace && !line.charAt(j).equals("@")) {
//      j += 1
//    }
//    if (i < size && j <= size) line.substring(i, j)
//    else throw new RuntimeException("Doing " + TITLE + ". Cannot find signature from procedure code: " + line)
//  }
//  
//  def getGlobalVarName(line : String) : String = {
//    val size = line.size
//    var i = if(line.contains("@@")) (line.indexOf("@@")) else size
//    while (i < size && line.charAt(i).isWhitespace) {
//      i += 1
//    }
//    var j = i
//    while (j < size && !line.charAt(j).isWhitespace && !line.charAt(j).equals("@")) {
//      j += 1
//    }
//    if (i < size && j <= size) line.substring(i, j)
//    else throw new RuntimeException("Doing " + TITLE + ". Cannot find global variable name from global code: " + line)
//  }
  
}