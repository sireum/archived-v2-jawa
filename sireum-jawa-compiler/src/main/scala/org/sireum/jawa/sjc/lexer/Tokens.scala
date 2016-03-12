/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.lexer

object Tokens {

  val CLASS_OR_INTERFACE = TokenType("CLASS_OR_INTERFACE")
  val METHOD = TokenType("METHOD")
  val STATIC_FIELD = TokenType("STATIC_FIELD")
  val EXTENDS_AND_IMPLEMENTS = TokenType("EXTENDS_AND_IMPLEMENTS")

  val CONST_CLASS = TokenType("CONST_CLASS")
  val LENGTH = TokenType("LENGTH")
  
  val EQUALS = TokenType("EQUALS")
  
  val ID = TokenType("ID")
  val LOCATION_ID = TokenType("LOCATION_ID")
  val STATIC_ID = TokenType("STATIC_ID")
  val EXCEPTION = TokenType("EXCEPTION")
  val INSTANCEOF = TokenType("INSTANCEOF")
    
  val NEW = TokenType("NEW")
  val THROW = TokenType("THROW")
  val CATCH = TokenType("CATCH")
  val IF = TokenType("IF")
  val THEN = TokenType("THEN")
  val GOTO = TokenType("GOTO")
  val SWITCH = TokenType("SWITCH")
  val ELSE = TokenType("ELSE")
  val RETURN = TokenType("RETURN")
  val CALL = TokenType("CALL")
  val MONITOR_ENTER = TokenType("MONITOR_ENTER")
  val MONITOR_EXIT = TokenType("MONITOR_EXIT")
  
  
  
  val EOF = TokenType("EOF")
  
  val LBRACKET = TokenType("LBRACKET")
  val RBRACKET = TokenType("RBRACKET")
  val LPAREN = TokenType("LPAREN")
  val RPAREN = TokenType("RPAREN")
  val LBRACE = TokenType("LBRACE")
  val RBRACE = TokenType("RBRACE")

  val STRING_LITERAL = TokenType("STRING_LITERAL")
  val FLOATING_POINT_LITERAL = TokenType("FLOATING_POINT_LITERAL")
  val INTEGER_LITERAL = TokenType("INTEGER_LITERAL")
  val CHARACTER_LITERAL = TokenType("CHARACTER_LITERAL")
  val TRUE = TokenType("TRUE")
  val FALSE = TokenType("FALSE")
  val NULL = TokenType("NULL")
  
  val COMMA = TokenType("COMMA")
  val DOT = TokenType("DOT")
  val SEMI = TokenType("SEMI")
  val COLON = TokenType("COLON")
  val ARROW = TokenType("ARROW")
  val AT = TokenType("AT")
  val RANGE = TokenType("RANGE")
  val ASSIGN_OP = TokenType("ASSIGN_OP")
  val HAT = TokenType("HAT")
  
  val CMP = TokenType("CMP")
//  val ANY = TokenType("ANY")
  
  val LINE_COMMENT = TokenType("LINE_COMMENT")
  val MULTILINE_COMMENT = TokenType("MULTILINE_COMMENT")
  val DOC_COMMENT = TokenType("DOC_COMMENT")
  
  val WS = TokenType("WS")
 
  val OP = TokenType("OP")
  
  val UNKNOWN = TokenType("UNKNOWN")
  
  val KEYWORDS = Set(
    CLASS_OR_INTERFACE, METHOD, STATIC_FIELD, EXTENDS_AND_IMPLEMENTS, IF, THEN, NEW,
    RETURN, THROW, CALL, SWITCH, ELSE, GOTO, CATCH, INSTANCEOF, CONST_CLASS,
    TRUE, FALSE, NULL, MONITOR_ENTER, MONITOR_EXIT, CMP, LENGTH)

  val COMMENTS = Set(LINE_COMMENT, MULTILINE_COMMENT, DOC_COMMENT)

  val IDS = Set(ID, LOCATION_ID, STATIC_ID)

  val LITERALS = Set(CHARACTER_LITERAL, INTEGER_LITERAL, FLOATING_POINT_LITERAL, STRING_LITERAL, TRUE, FALSE, NULL)

}

