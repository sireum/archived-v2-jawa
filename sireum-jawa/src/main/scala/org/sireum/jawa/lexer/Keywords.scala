package org.sireum.jawa.lexer

import org.sireum.jawa.lexer.Tokens._

object Keywords {

  def apply(s: String): Option[TokenType] = keywords get s

  private val keywords = Map(
    "else" -> ELSE,
    "throw" -> THROW,
    "switch" -> SWITCH,
    "if" -> IF,
    "goto" -> GOTO,
    "extends" -> EXTENDS,
    "procedure" -> METHOD,
    "true" -> TRUE,
    "return" -> RETURN,
    "record" -> CLASS,
    "catch" -> CATCH,
    "then" -> THEN,
    "global" -> STATIC_FIELD,
    "false" -> FALSE,
    "null" -> NULL,
    "call" -> CALL,
    "new" -> NEW)

}
