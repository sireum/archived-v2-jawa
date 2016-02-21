/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.lexer

import org.sireum.jawa.sjc.lexer.Tokens._

object Keywords {

  def apply(s: String): Option[TokenType] = keywords get s

  private val keywords = Map(
    "else" -> ELSE,
    "throw" -> THROW,
    "switch" -> SWITCH,
    "if" -> IF,
    "goto" -> GOTO,
    "extends" -> EXTENDS_AND_IMPLEMENTS,
    "procedure" -> METHOD,
    "true" -> TRUE,
    "return" -> RETURN,
    "record" -> CLASS_OR_INTERFACE,
    "catch" -> CATCH,
    "then" -> THEN,
    "global" -> STATIC_FIELD,
    "false" -> FALSE,
    "null" -> NULL,
    "call" -> CALL,
    "new" -> NEW)

}
