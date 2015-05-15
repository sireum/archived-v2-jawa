/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.lexer

import org.sireum.jawa.sjc.parser.JawaParserException
import org.sireum.jawa.sjc.util.Position

class JawaLexerException(pos: Position, message: String) extends JawaParserException(pos, message)