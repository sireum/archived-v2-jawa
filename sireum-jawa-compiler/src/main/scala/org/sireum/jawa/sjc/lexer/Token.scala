/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.lexer

import org.sireum.jawa.sjc.lexer.Tokens._
import org.sireum.jawa.sjc.util.Range
import org.sireum.util.FileResourceUri
import org.sireum.jawa.sjc.util.RangePosition
import org.sireum.jawa.sjc.util.SourceFile
import org.sireum.jawa.sjc.io.AbstractFile

/**
 * A token of Pilar source.
 *
 * @param rawText -- the text associated with the token
 */
case class Token(tokenType: TokenType, pos: RangePosition, rawtext: String) {

  private[lexer] var associatedWhitespaceAndComments_ : HiddenTokens = null

  private[lexer] var containsUnicodeEscape = false

  def associatedWhitespaceAndComments: HiddenTokens = associatedWhitespaceAndComments_
  
  def line: Int = pos.line
  
  def column: Int = pos.column
  
  def offset: Int = pos.point
  
  def length: Int = rawtext.length

  def lastCharacterOffset: Int = pos.point + length - 1
  
  def file: SourceFile = pos.source
  
  def range: Range = Range(pos.start, pos.end - pos.start + 1)
  
  def text: String = {
    tokenType match {
      case ID =>
        rawtext.replace("`", "")
      case _ => rawtext
    }
  }
  
  override def toString(): String = {
    var txt = text
    if (txt != null) {
      txt = txt.replace("\n","\\n")
      txt = txt.replace("\r","\\r")
      txt = txt.replace("\t","\\t")
    }
    else {
      txt = "<no text>"
    }
    return "["+txt+"',<"+tokenType+">"+"@"+pos+"]";
  }

}