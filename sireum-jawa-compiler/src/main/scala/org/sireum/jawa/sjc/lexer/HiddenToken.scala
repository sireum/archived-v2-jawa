/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.lexer

abstract sealed class HiddenToken(val token: Token) {

  lazy val newlineful = token.text contains '\n'

  def text = token.text

}

case class Whitespace(override val token: Token) extends HiddenToken(token)

sealed abstract class Comment(token: Token) extends HiddenToken(token)

object Comment {

  def unapply(comment: Comment) = Some(comment.token)

}

case class SingleLineComment(override val token: Token) extends Comment(token)

case class MultiLineComment(override val token: Token) extends Comment(token)

case class DocComment(override val token: Token) extends Comment(token)