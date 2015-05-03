package org.sireum.jawa.sjc.lexer

import org.sireum.jawa._
import org.sireum.jawa.sjc.lexer.Tokens._
import org.scalatest.FlatSpec
import org.scalatest._

import java.io._

/**
 * Test full tokeniser.
 */
class WhitespaceAndCommentsGrouperTest extends FlatSpec with ShouldMatchers {

  implicit def string2TestString(s: String) =
    new TestString(s)

  """
     #L1.   switch  v7
                 | 1 => goto Lx
                 | else => goto Ly;""" shouldProduceTokens (
    LOCATION_ID, SWITCH, ID,
    OP, INTEGER_LITERAL, ARROW, GOTO, ID,
    OP, ELSE, ARROW, GOTO, ID, SEMI)

  class TestString(s: String) {

    def shouldProduceTokens(toks: TokenType*)() {
      check(s, toks.toList)
    }

    private def check(s: String, expectedTokens: List[TokenType]) {
      it should ("tokenise >>>" + s + "<<< as >>>" + expectedTokens + "<<<") in {
        val actualTokens: List[Token] = JawaLexer.tokenise(Left(s))
        val actualTokenTypes = actualTokens.map(_.tokenType)
        require(actualTokenTypes.last == EOF, "Last token must be EOF, but was " + actualTokens.last.tokenType)
        require(actualTokenTypes.count(_ == EOF) == 1, "There must only be one EOF token")
        val reconstitutedSource = actualTokens.init.map(_.text).mkString
        require(actualTokenTypes.init == expectedTokens, "Tokens do not match. Expected " + expectedTokens + ", but was " + actualTokenTypes.init)
      }
    }

  }

}

