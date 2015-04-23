/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.lexer

import org.sireum.pilar.parser.Antlr4PilarLexer
import org.sireum.jawa.lexer.Tokens._
import java.io._
import org.sireum.util._
import org.sireum.pilar.parser.Parser
import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.{Token => AntlrToken}
import java.net.URI

class JawaLexer(aplexer: Antlr4PilarLexer) extends Iterator[Token] {
  
  private var eofTokenEmitted = false
  protected var builtToken: Token = _
  
  import org.sireum.pilar.parser.Antlr4PilarLexer._
  protected def fetchPilarToken() = {
    val aptoken: AntlrToken = aplexer.nextToken()
    val tokenType =
      aptoken.getType match {
        case T__1 =>   // )
          Tokens.RPAREN
        case T__3 =>   // else
          Tokens.ELSE
        case T__6 =>   // =>
          Tokens.ARROW
        case T__9 =>   // [
          Tokens.LBRACKET
        case T__10 =>  // :
          Tokens.COLON
        case T__13 =>  // throw
          Tokens.THROW
        case T__16 =>  // .
          Tokens.DOT
        case T__17 =>  // switch
          Tokens.SWITCH
        case T__20 =>  // if
          Tokens.IF
        case T__21 =>  // }
          Tokens.RBRACE
        case T__22 =>  // extends
          Tokens.EXTENDS_AND_IMPLEMENTS
        case T__23 =>  // goto
          Tokens.GOTO
        case T__25 =>  // ;
          Tokens.SEMI
        case T__27 =>  // procedure
          Tokens.METHOD
        case T__28 =>  // return
          Tokens.RETURN
        case T__29 =>  // true
          Tokens.TRUE
        case T__30 =>  // record
          Tokens.CLASS_OR_INTERFACE
        case T__31 =>  // catch
          Tokens.CATCH
        case T__33 =>  // then
          Tokens.THEN
        case T__35 =>  // @
          Tokens.AT
        case T__37 =>  // ]
          Tokens.RBRACKET
        case T__40 =>  // global
          Tokens.STATIC_FIELD
        case T__44 =>  // false
          Tokens.FALSE
        case T__45 =>  // ,
          Tokens.COMMA
        case T__46 =>  // (
          Tokens.LPAREN
        case T__47 =>  // null
          Tokens.NULL
        case T__48 =>  // call
          Tokens.CALL
        case T__50 =>  // =
          Tokens.EQUALS
        case T__51 =>  // ..
          Tokens.RANGE
        case T__52 =>  // {
          Tokens.LBRACE
        case T__53 =>  // new
          Tokens.NEW
        case GID =>
          Tokens.STATIC_ID
        case ID =>
          if(aptoken.getText == "cmp" || aptoken.getText == "cmpl" || aptoken.getText == "cmpg")
            Tokens.CMP
          else Tokens.ID
        case LID =>
          Tokens.LOCATION_ID
        case MSTRING
           | STRING
           =>
          Tokens.STRING_LITERAL
        case WS =>
          Tokens.WS
        case COMMENT =>
          Tokens.MULTILINE_COMMENT
        case LINE_COMMENT =>
          Tokens.LINE_COMMENT
        case HEX
           | DEC
           | OCT
           | BIN
           =>
          Tokens.INTEGER_LITERAL
        case FLOAT =>
          Tokens.FLOATING_POINT_LITERAL
        case CHAR =>
          Tokens.CHARACTER_LITERAL
        case AssignOP =>
          Tokens.ASSIGN_OP
        case T__7      // >
           | T__8      // |
           | T__38     // <
           | CondAndOP
           | CondOrOP
           | AndOP
           | XorOP
           | OrOP
           | EqOP
           | RelOP
           | ShiftOP
           | AddOP
           | MulOP
           | UnaryOP
           =>
          Tokens.OP
        case AntlrToken.EOF =>
          Tokens.EOF
        case T__0      // typealias
           | T__2      // typedef
           | T__4      // in
           | T__5      // start
           | T__11     // expdef
           | T__12     // ...
           | T__14     // ->
           | T__15     // +>
           | T__18     // assume
           | T__19     // enum
           | T__24     // ==>
           | T__26     // <==
           | T__32     // procdef
           | T__34     // let
           | T__36     // assert
           | T__39     // extension
           | T__41     // const
           | T__42     // -!>
           | T__43     // actiondef
           | T__49     // ^
           | T__54     // fun
           | ErrorChar
           => 
           throw new JawaLexerException("Unexpected token: " + aptoken)
        case _ =>
          throw new JawaLexerException("Unexpected token: " + aptoken)
      }
    
    val tokenLine = aptoken.getLine
    val tokenOffset = aptoken.getCharPositionInLine
    val rawText = aptoken.getText
    token(tokenType, tokenLine, tokenOffset, rawText)
  }
  
  /**
   * Mark the end of a token of the given type.
   */
  protected def token(tokenType: TokenType, tokenLine: Int, tokenOffset: Int, rawText: String) {
    builtToken = Token(tokenType, tokenLine, tokenOffset, rawText)
  }
  
  private[lexer] def text = aplexer.getText()
  
  def next(): Token = {
    fetchPilarToken()

    if (builtToken.tokenType == EOF)
      eofTokenEmitted = true
    builtToken
  }

  def hasNext = !eofTokenEmitted
}

object JawaLexer {

  /**
   * Convert the given Pilar source code into a list of "raw" tokens.
   *
   * This includes whitespace and comment tokens. No NEWLINE or NEWLINES tokens are inferred. The final token
   * will be of type EOF.
   */
  @throws(classOf[JawaLexerException])
  def rawTokenise(source: Either[String, ResourceUri]): List[Token] =
    createRawLexer(source).toList

  /**
   * Create a lexer for "raw" tokens.
   *
   * @see rawTokenise
   */
  def createRawLexer(source: Either[String, ResourceUri]): JawaLexer = {
    val reader = source match {
      case Left(text)     => new StringReader(text)
      case Right(fileUri) => new FileReader(new File(new URI(fileUri)))
    }
    val input = new ANTLRInputStream(reader)
    val aplexer = new Antlr4PilarLexer(input)
    makeRawLexer(aplexer)
  }
    
  def makeRawLexer(aplexer: Antlr4PilarLexer): JawaLexer =
    new JawaLexer(aplexer)

  /**
   * Convert the given Pilar source code into a list of tokens.
   *
   * whitespace and comments are absorbed into the token they
   * precede. The final token will be of type EOF.
   *
   * @param forgiveErrors -- if true, no exceptions will be thrown when malformed tokens are encountered.
   * @param pilarVersion -- the version of Pilar to assume as the source type (e.g. "4.0"). This can affect the
   *   interpretation of certain tokens (for example, floating point literals).
   */
  @throws(classOf[JawaLexerException])
  def tokenise(source: Either[String, ResourceUri]): List[Token] = {
    val rawLexer = createRawLexer(source)
    val lexer = new WhitespaceAndCommentsGrouper(rawLexer)
    lexer.toList
  }

}