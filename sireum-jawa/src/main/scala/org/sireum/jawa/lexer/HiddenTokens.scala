package org.sireum.jawa.lexer

import org.sireum.jawa.lexer.Tokens._

object NoHiddenTokens extends HiddenTokens(Nil)

case class HiddenTokens(tokens: List[HiddenToken]) extends Iterable[HiddenToken] {

  def removeInitialWhitespace = new HiddenTokens(tokens.dropWhile(_.isInstanceOf[Whitespace]))

  def iterator: Iterator[HiddenToken] = tokens.iterator

  val comments: List[Comment] = tokens collect { case comment: Comment ⇒ comment }

  val whitespaces: List[Whitespace] = tokens collect { case whitespace @ Whitespace(_) ⇒ whitespace }

  def firstTokenOption = tokens.headOption

  def lastTokenOption = tokens.lastOption

  def containsNewline = text contains '\n'

  def containsComment = comments.nonEmpty

  def containsUnicodeEscape: Boolean = {
    for (token ← tokens if token.token.containsUnicodeEscape)
      return true
    false
  }

  lazy val text: String = {
    val sb = new StringBuilder
    for (token ← tokens) sb.append(token.text)
    sb.toString
  }
  
  def rawTokens = tokens.map(_.token)

  def offset = tokens.head.token.offset

  def lastCharacterOffset = tokens.last.token.lastCharacterOffset

}