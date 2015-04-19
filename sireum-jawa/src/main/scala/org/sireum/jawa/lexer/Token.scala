package org.sireum.jawa.lexer

import org.sireum.jawa.lexer.Tokens._
import org.sireum.jawa.util.Range

/**
 * A token of Pilar source.
 *
 * @param rawText -- the text associated with the token
 */
case class Token(tokenType: TokenType, line: Int, offset: Int, text: String) {

  private[lexer] var associatedWhitespaceAndComments_ : HiddenTokens = null

  private[lexer] var containsUnicodeEscape = false

  def associatedWhitespaceAndComments: HiddenTokens = associatedWhitespaceAndComments_
  
  def length = text.length

  def range = Range(offset, length)

  def lastCharacterOffset = offset + length - 1
  
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
    return "["+txt+"',<"+tokenType+">"+","+line+":"+offset+"]";
  }

}