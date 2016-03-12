/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa

import java.io.LineNumberReader
import java.io.StringReader
import java.io.Reader
import org.sireum.util.ISet

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object LightWeightPilarParser {
  val TITLE = "LightWeightPilarParser"
    
  val DEBUG = false
  
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
  
  def splitCode(code: String): ISet[String] = {
    code.replaceAll("(record `)", "DELIMITER$1").split("DELIMITER").tail.toSet
  }
  
  def getCode(recordCode : String, contentSig : String) : Option[String] = {
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
  
  def getClassName(line : String) : String = {
    val size = line.size
    var i = if(line.contains("record")) (line.indexOf("record") + 7) else size
    while (i < size && line.charAt(i).isWhitespace) {
      i += 1
    }
    var j = i
    while (j < size && !line.charAt(j).isWhitespace && !line.charAt(j).equals("@")) {
      j += 1
    }
    if (i < size && j <= size) line.substring(i + 1, j - 1)
    else throw new RuntimeException("Doing " + TITLE + ". Cannot find name from record code: \n" + line)
  }
}
