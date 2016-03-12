/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa.sjc.astselect

import scala.util.control.Exception._
import org.sireum.jawa.sjc.lexer._
import org.sireum.jawa.sjc.parser._
import org.sireum.jawa.sjc.parser.JawaParserException
import org.sireum.jawa.DefaultReporter
import org.sireum.jawa.io.Range

/**
 * @author fgwei
 */
object AstSelector {
  /**
   * Expands the given selection in the source to the range of the closest appropriate
   * enclosing AST element. Returns None if the source does not parse correctly, or if
   * there is no strictly larger containing AST element.
   */
  def expandSelection(source: String, initialSelection: Range): Option[Range] =
    catching(classOf[JawaParserException]).toOption {
      new AstSelector(source).expandSelection(initialSelection)
    }
}

class AstSelector(source: String) {
  import AstSelector._
  val reporter = new DefaultReporter
  
  private val tokens = JawaLexer.tokenise(Left(source),reporter )
  
  private val compilationUnitOpt: Option[CompilationUnit] = {
    JawaParser.parse(tokens, true, reporter)
  }
  
  private val allTokens: List[Token] = tokens.flatMap { token =>
    token.associatedWhitespaceAndComments.rawTokens :+ token
  }

  private def previousToken(token: Token): Option[Token] =
    tokens.indexOf(token) match {
      case 0 | -1 => None
      case n      => Some(tokens(n - 1))
    }
  
  def expandSelection(initialSelection: Range): Option[Range] =
    expandToToken(initialSelection)
      
  /**
   * If the selection is a strict subrange of some token, expand to the entire token.
   */
  private def expandToToken(initialSelection: Range): Option[Range] =
    allTokens.find { token =>
      isSelectableToken(token) && (token.range contains initialSelection) && initialSelection.length < token.length
    }.map(_.range)
    
//  private def findAssociatedAstNode: Option[JawaAstNode] =
//    compilationUnitOpt.flatMap { cu => findAssociatedAstNode(cu) }
//
//  private def findAssociatedAstNode(nodeToSearch: JawaAstNode): Option[JawaAstNode] =
//    nodeToSearch.firstTokenOption flatMap { firstToken =>
//      val hiddenTokens = getPriorHiddenTokens(firstToken)
//      if (hiddenTokens.rawTokens.contains(scaladocCommentToken) && !nodeToSearch.isInstanceOf[CompilationUnit])
//        Some(nodeToSearch)
//      else {
//        for {
//          childNode <- nodeToSearch.immediateChildren
//          result <- findAssociatedAstNode(childNode)
//        } return Some(result)
//        None
//      }
//    }
  
  /**
   * Attempt to find a suitable AST node to expand to which contains the given selection.
   *
   * @param enclosingNodes -- stack of nodes recording path to root compilation unit (useful for more context-aware
   *   decisions about whether to expand to a node or not).
   */
//  private def expandToEnclosingAst(node: JawaAstNode, initialSelection: Range, enclosingNodes: List[JawaAstNode]): Option[Range] = {
//
//    val nodeRange = adjustedNodeRange(node).getOrElse { return None }
//
//    if (!nodeRange.contains(initialSelection)) { return None }
//
//    for {
//      childNode <- node.immediateChildren
//      descendantRange <- expandToEnclosingAst(childNode, initialSelection, enclosingNodes = node :: enclosingNodes)
//    } return Some(descendantRange)
//
//    if (nodeRange.strictlyContains(initialSelection) && isSelectableAst(node :: enclosingNodes))
//      Some(nodeRange)
//    else
//      None
//
//  }
  
  private def isSelectableToken(token: Token) = {
    val tokenType = token.tokenType
    import tokenType._
    isLiteral || isId
  }
  
  private def getPriorHiddenTokens(token: Token) = token.associatedWhitespaceAndComments

}
