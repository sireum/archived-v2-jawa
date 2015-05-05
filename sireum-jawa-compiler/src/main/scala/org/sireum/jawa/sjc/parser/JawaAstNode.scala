/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.parser

import scala.collection._
import org.sireum.pilar._
import org.sireum.jawa.sjc.lexer.Token
import org.sireum.jawa.sjc.util.CaseClassReflector
import org.sireum.util._
import org.sireum.jawa.sjc.JawaType
import org.sireum.jawa.sjc.JavaKnowledge

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
sealed trait JawaAstNode extends CaseClassReflector with JavaKnowledge {

  def tokens: IList[Token]

  def firstTokenOption: Option[Token] = tokens.headOption

  lazy val lastTokenOption: Option[Token] = tokens.lastOption

  def firstToken = firstTokenOption.get

  lazy val lastToken = lastTokenOption.get

  protected trait Flattenable {
    def tokens: IList[Token]
  }

  def isEmpty = tokens.isEmpty

  protected implicit def astNodeToFlattenable(node: JawaAstNode): Flattenable = new Flattenable { val tokens = node.tokens }
  protected implicit def listToFlattenable[T <% Flattenable](list: IList[T]): Flattenable = new Flattenable { val tokens = list flatMap { _.tokens } }
  protected implicit def optionToFlattenable[T <% Flattenable](option: Option[T]): Flattenable = new Flattenable { val tokens = option.toList flatMap { _.tokens } }
  protected implicit def pairToFlattenable[T1 <% Flattenable, T2 <% Flattenable](pair: (T1, T2)): Flattenable = new Flattenable { val tokens = pair._1.tokens ::: pair._2.tokens }
  protected implicit def tripleToFlattenable[T1 <% Flattenable, T2 <% Flattenable, T3 <% Flattenable](triple: (T1, T2, T3)): Flattenable = new Flattenable { val tokens = triple._1.tokens ++ triple._2.tokens ++ triple._3.tokens }
  protected implicit def eitherToFlattenable[T1 <% Flattenable, T2 <% Flattenable](either: T1 Either T2): Flattenable = new Flattenable {
    val tokens = either match {
      case Left(f)  ⇒ f.tokens
      case Right(f) ⇒ f.tokens
    }
  }
  protected implicit def tokenToFlattenable(token: Token): Flattenable = new Flattenable { val tokens = List(token) }

  protected def flatten(flattenables: Flattenable*): IList[Token] = flattenables.toList flatMap { _.tokens }

  def immediateChildren: IList[JawaAstNode] = productIterator.toList flatten immediateAstNodes

  private def immediateAstNodes(n: Any): IList[JawaAstNode] = n match {
    case a: JawaAstNode            ⇒ List(a)
    case t: Token                  ⇒ Nil
    case Some(x)                   ⇒ immediateAstNodes(x)
    case xs @ (_ :: _)             ⇒ xs flatMap { immediateAstNodes(_) }
    case Left(x)                   ⇒ immediateAstNodes(x)
    case Right(x)                  ⇒ immediateAstNodes(x)
    case (l, r)                    ⇒ immediateAstNodes(l) ++ immediateAstNodes(r)
    case (x, y, z)                 ⇒ immediateAstNodes(x) ++ immediateAstNodes(y) ++ immediateAstNodes(z)
    case true | false | Nil | None ⇒ Nil
  }

  /**
   * Returns range of tokens in the node, or None if there are no tokens in the node
   */
  def rangeOpt: Option[Range] =
    if (tokens.isEmpty)
      None
    else {
      val firstIndex = tokens.head.offset
      val lastIndex = tokens.last.lastCharacterOffset
      Some(Range(firstIndex, lastIndex - firstIndex + 1))
    }

}

sealed trait ParsableAstNode extends JawaAstNode

case class CompilationUnit(
    topDecls: IList[ClassOrInterfaceDeclaration], 
    eofToken: Token) extends ParsableAstNode {
  lazy val tokens = flatten(topDecls, eofToken)
}

sealed trait Declaration extends JawaAstNode {
  def annotations: IList[Annotation]
  def accessModifier: String = {
    annotations.find { a => a.key == "AccessFlag" || a.key == "Access" } match{
      case Some(a) => a.value
      case None => ""
    }
  }
}

case class ClassOrInterfaceDeclaration(
    dclToken: Token, 
    nameID: Token, 
    annotations: IList[Annotation], 
    extendsAndImplimentsClausesOpt: Option[ExtendsAndImplimentsClauses],
    instanceFieldDeclarationBlock: InstanceFieldDeclarationBlock,
    staticFields: IList[StaticFieldDeclaration],
    methods: IList[MethodDeclaration]) extends Declaration with ParsableAstNode {
  lazy val tokens = flatten(dclToken, nameID, annotations, extendsAndImplimentsClausesOpt, instanceFieldDeclarationBlock, staticFields, methods)
  def isInterface: Boolean = {
    annotations.exists { a => a.key == "type" && a.value == "interface" }
  }
  def parents: IList[String] = extendsAndImplimentsClausesOpt match {case Some(e) => e.parents; case None => ilistEmpty}
  def instanceFields: IList[InstanceFieldDeclaration] = instanceFieldDeclarationBlock.instanceFields
}

case class Annotation(
    at: Token, 
    annotationID: Token, 
    annotationValueOpt: Option[Token]) extends JawaAstNode {
  lazy val tokens = flatten(at, annotationID, annotationValueOpt)
  def key: String = annotationID.text
  def value: String = 
    annotationValueOpt match{
      case Some(a) => a.text 
      case None => ""
    }
}

case class ExtendsAndImplimentsClauses(
    extendsAndImplementsToken: Token,
    firstParentID: Token,
    restParentIDs: IList[(Token, Token)]) extends JawaAstNode {
  lazy val tokens = flatten(extendsAndImplementsToken, firstParentID, restParentIDs)
  def parents: IList[String] = firstParentID.text :: restParentIDs.map(_._2.text)
}

sealed trait Field extends JawaAstNode {
  def typ: Type
  def nameID: Token
  def FQN: String
  def fieldName: String = getFieldNameFromFieldFQN(FQN)
  def isStatic: Boolean
}

case class InstanceFieldDeclarationBlock(
    lbrace: Token,
    instanceFields: IList[InstanceFieldDeclaration],
    rbrace: Token) extends JawaAstNode {
  lazy val tokens = flatten(lbrace, instanceFields, rbrace)
}

case class InstanceFieldDeclaration(
    typ: Type, 
    nameID: Token, 
    annotations: IList[Annotation], 
    semi: Token) extends Field with Declaration {
  lazy val tokens = flatten(typ, nameID, annotations, semi)
  def FQN: String = nameID.text
  def isStatic: Boolean = false
}

case class StaticFieldDeclaration(
    staticFieldToken: Token, 
    typ: Type, 
    nameID: Token, 
    annotations: IList[Annotation], 
    semi: Token) extends Field with Declaration {
  lazy val tokens = flatten(staticFieldToken, typ, nameID, annotations, semi)
  def FQN: String = nameID.text.replace("@@", "")
  def isStatic: Boolean = true
}

case class Type(baseTypeID: Token, typeFragments: IList[TypeFragment]) extends JawaAstNode {
  lazy val tokens = flatten(baseTypeID, typeFragments)
  def dimentions: Int = typeFragments.size
  def baseTypeName: String = baseTypeID.text
  def typ: JawaType = getType(baseTypeName, dimentions)
}

sealed trait TypeFragment extends JawaAstNode

case class RawTypeFragment(lbracket: Token, rbracket: Token) extends TypeFragment {
  lazy val tokens = flatten(lbracket, rbracket)
}

case class TypeFragmentWithInit(lbracket: Token, varIDs: IList[(Token, Option[Token])], rbracket: Token) extends TypeFragment {
  lazy val tokens = flatten(lbracket, varIDs, rbracket)
}

case class MethodDeclaration(
    dclToken: Token,
    returnType: Type,
    nameID: Token,
    paramClause: ParamClause,
    annotations: IList[Annotation],
    body: Body) extends Declaration with ParsableAstNode {
  lazy val tokens = flatten(dclToken, returnType, nameID, paramClause)
  def owner: String = annotations.find { a => a.key == "owner" }.get.value
  def signature: String = annotations.find { a => a.key == "signature" }.get.value
}

case class ParamClause(
    lparen: Token,
    params: IList[(Param, Option[Token])], 
    rparen: Token) extends JawaAstNode {
  lazy val tokens = flatten(lparen, params, rparen)
  def param(i: Int): Param =
    i match {
      case n if (n >= 0 && n < params.size) => params(n)._1
      case _ => throw new IndexOutOfBoundsException("List size " + params.size + " but index " + i)
    }
  def paramlist: IList[Param] = params.map(_._1)
}

case class Param(
    typ: Type, 
    nameID: Token, 
    annotations: IList[Annotation]) extends JawaAstNode {
  lazy val tokens = flatten(typ, nameID, annotations)
  def isThis: Boolean = annotations.exists { a => a.key == "type" && a.value == "this" }
  def isObject: Boolean = annotations.exists { a => a.key == "type" && (a.value == "this" || a.value == "object") }
  def name: String = nameID.text
}

case class Body(
    lbrace: Token, 
    locals: IList[LocalVarDeclaration], 
    locations: IList[Location], 
    catchClauses: IList[CatchClause], 
    rbrace: Token) extends JawaAstNode {
  lazy val tokens = flatten(lbrace, locals, locations, catchClauses, rbrace)
  def isEmptyBody: Boolean = locations.isEmpty || catchClauses.isEmpty
}

case class LocalVarDeclaration(
    nameID: Token, 
    annotations: IList[Annotation], 
    semi: Token) extends Declaration {
  lazy val tokens = flatten(nameID, annotations, semi)
}

case class Location(
    locationID: Token, 
    statement: Statement, 
    semiOpt: Option[Token]) extends ParsableAstNode {
  lazy val tokens = flatten(locationID, statement, semiOpt)
  def locationUri: String = locationID.text.substring(1, locationID.length - 1)
  var locationIndex: Int = 0
}

sealed trait Statement extends JawaAstNode

case class CallStatement(
    callToken: Token, 
    lhs: Expression, 
    assignOP: Token,
    invokeID: Token,
    argClause: ArgClause,
    annotations: IList[Annotation]) extends Statement {
  lazy val tokens = flatten(callToken, lhs, assignOP, invokeID, argClause, annotations)
  def typ: String = annotations.find { a => a.key == "type" }.get.value
  def signature: String = annotations.find { a => a.key == "signature" }.get.value
  def classDescriptor: String = annotations.find { a => a.key == "classDescriptor" }.get.value
  def isStatic: Boolean = typ == "static"
  def isVirtual: Boolean = typ == "virtual"
  def isSuper: Boolean = typ == "super"
  def isDirect: Boolean = typ == "direct"
}

case class ArgClause(
    lparen: Token, 
    varIDs: IList[(Token, Option[Token])], 
    rparen: Token) extends JawaAstNode {
  lazy val tokens = flatten(lparen, varIDs, rparen)
  def arg(i: Int): String =
    i match {
      case n if (n >= 0 && n < varIDs.size) => varIDs(n)._1.text
      case _ => throw new IndexOutOfBoundsException("List size " + varIDs.size + " but index " + i)
    }
}

case class AssignmentStatement(
    lhs: Expression,
    assignOP: Token,
    rhs: Expression,
    annotations: IList[Annotation]) extends Statement {
  lazy val tokens = flatten(lhs, assignOP, rhs, annotations)
}

case class ThrowStatement(
    throwToken: Token,
    variableID: Token) extends Statement {
  lazy val tokens = flatten(throwToken, variableID)
}

case class IfStatement(
    ifToken: Token,
    exp: Expression,
    thengoto: (Token, Token),
    targetLocation: Token) extends Statement {
  lazy val tokens = flatten(ifToken, exp, thengoto, targetLocation)
}

case class GotoStatement(
    goto: Token,
    targetLocation: Token) extends Statement {
  lazy val tokens = flatten(goto, targetLocation)
}

case class SwitchStatement(
    switchToken: Token,
    condition: Token,
    cases: IList[SwitchCase],
    defaultCaseOpt: Option[SwitchDefaultCase]) extends Statement {
  lazy val tokens = flatten(switchToken, condition, cases, defaultCaseOpt)
}

case class SwitchCase(
    bar: Token,
    constant: Token,
    arrow: Token,
    goto: Token,
    targetLocation: Token) extends JawaAstNode {
  lazy val tokens = flatten(bar, constant, arrow, goto, targetLocation)
}

case class SwitchDefaultCase(
    bar: Token,
    elseToken: Token,
    arrow: Token,
    goto: Token,
    targetLocation: Token) extends JawaAstNode {
  lazy val tokens = flatten(bar, elseToken, arrow, goto, targetLocation)
}

case class ReturnStatement(
    returnToken: Token,
    expOpt: Option[Expression],
    annotations: IList[Annotation]) extends Statement {
  lazy val tokens = flatten(returnToken, expOpt, annotations)
}

case class EmptyStatement(
    annotations: IList[Annotation]) extends Statement {
  lazy val tokens = flatten(annotations)
}

sealed trait Expression extends JawaAstNode

case class NameExpression(
    nameID: Token,
    annotations: IList[Annotation]) extends Expression {
  lazy val tokens = flatten(nameID, annotations)
  def name: String = nameID.text
  def isStatic: Boolean = name.contains("@@")
}

case class IndexingExpression(
    baseID: Token,
    indices: IList[IndexingSuffix]) extends Expression {
  lazy val tokens = flatten(baseID, indices)
  def dimentions: Int = indices.size
}

case class IndexingSuffix(
    lbracket: Token,
    exp: Expression,
    rbracket: Token) extends JawaAstNode {
  lazy val tokens = flatten(lbracket, exp, rbracket)
}
    
case class AccessExpression(
    baseID: Token,
    dot: Token,
    fieldID: Token) extends Expression {
  lazy val tokens = flatten(baseID, dot, fieldID)
}

case class TupleExpression(
    lparen: Token,
    constants: IList[(Token, Option[Token])],
    rparen: Token) extends Expression {
  lazy val tokens = flatten(lparen, constants, rparen)
}

case class CastExpression(
    lparen: Token,
    typ: Type,
    rparen: Token,
    exp: Expression)
    extends Expression {
  lazy val tokens = flatten(lparen, typ, rparen, exp)
}

case class NewExpression(
    newToken: Token,
    typ: Type) extends Expression {
  lazy val tokens = flatten(newToken, typ)
}

case class LiteralExpression(
  constant : Token) extends Expression {
  lazy val tokens = flatten(constant)
}

case class UnaryExpression(
  op: Token,
  exp: Expression)
    extends Expression {
  lazy val tokens = flatten(op, exp)
}

case class BinaryExpression(
  left: Token,
  op: Token,
  right: Token)
    extends Expression {
  lazy val tokens = flatten(left, op, right)
}

case class CmpExpression(
    cmp: Token,
    lparen: Token,
    var1ID: Token,
    comma: Token,
    var2ID: Token,
    rparen: Token) extends Expression {
  lazy val tokens = flatten(cmp, lparen, var1ID, comma, var2ID, rparen)
}

case class CatchClause(
    catchToken: Token,
    typ: Type,
    range: CatchRange,
    goto: Token,
    targetLocation: Token,
    semi: Token) extends JawaAstNode {
  lazy val tokens = flatten(catchToken, typ, range, goto, targetLocation, semi)
}

case class CatchRange(
    at: Token,
    lbracket: Token,
    fromLocation: Token,
    range: Token,
    toLocation: Token,
    rbracket: Token) extends JawaAstNode {
  lazy val tokens = flatten(at, lbracket, fromLocation, range, toLocation, rbracket)
}