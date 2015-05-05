/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.util

import java.net._
import java.io._
import org.sireum.util._
import org.sireum.jawa.sjc.interactive.JawaCodeSource
import org.sireum.jawa.sjc.Signature
import org.sireum.jawa.sjc.JavaKnowledge
import org.sireum.jawa.sjc.ObjectType

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object LightWeightPilarParser extends JavaKnowledge {

  val TITLE = "LightWeightPilarParser"
    
  val DEBUG = false
  
  def apply(fileResourceUri: FileResourceUri, determiner: Either[JawaCodeSource.CodeType.Value, LibraryAPISummary]) =
    parseModel(fileResourceUri, determiner)

  def parseModel(fileResourceUri: FileResourceUri, determiner: Either[JawaCodeSource.CodeType.Value, LibraryAPISummary]) = {
    val fr = new FileReader(new File(new URI(fileResourceUri)))
    try readModelChunks(fileResourceUri: FileResourceUri, fr, determiner) finally fr.close
    if(DEBUG)
    	JawaCodeSource.printContent
  }
  
  def getEmptyBodyCode(recordCode: String): String = {
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
  
  def getCode(recordCode: String, contentSig: Signature): Option[String] = {
    val lnr = new LineNumberReader(new StringReader(recordCode))
    var lineNo = 0

    var chunkLineNo = 0
    val sb = new StringBuilder

    var lineText = lnr.readLine

    val keywords = Set("record", "global", "procedure")

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
   * read the code of each "record" and stores it in JawaCodeSource as a map: (className -> code). 
   * Note that each record block in pilar source starts with keyword "record"; 
   * so, the searching starts with keyword "record" and goes on until find another "record". The procs inside a record x are included in the code of x.
   */
  def readModelChunks(fileResourceUri: FileResourceUri, r: Reader, determiner: Either[JawaCodeSource.CodeType.Value, LibraryAPISummary]) = {
    val lnr = new LineNumberReader(r)
    var lineNo = 0

    var chunkLineNo = 0
    var sb = new StringBuilder

    var lineText = lnr.readLine

    val keyword = "record"

    var className: String = null
    while (lineText != null) {
      val word = getFirstWord(lineText)

      if (keyword == word) {
        if(className!=null)
        	JawaCodeSource.setClassCode(getTypeFromName(className).asInstanceOf[ObjectType], fileResourceUri, sb.toString, determiner)
        className = getClassName(lineText)

        sb = new StringBuilder
        chunkLineNo = lineNo
      }

      sb.append(lineText)
      sb.append('\n')
      lineNo += 1

      lineText = lnr.readLine
    }
    JawaCodeSource.setClassCode(getTypeFromName(className).asInstanceOf[ObjectType], fileResourceUri, sb.toString, determiner)
  }

  def getFirstWord(line: String) = {
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
  
  def getClassName(line: String): String = {
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

}