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
import org.sireum.jawa.sjc.Signature
import org.sireum.jawa.sjc.ObjectType
import org.sireum.jawa.sjc.lexer.TokenType
import org.sireum.jawa.sjc.lexer.Tokens
import org.sireum.jawa.sjc.util.Range
import org.sireum.jawa.sjc.util.Position
import org.sireum.jawa.sjc.util.NoPosition

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
sealed trait JawaAstNode extends CaseClassReflector with JavaKnowledge {

  def tokens: IList[Token]

  def firstTokenOption: Option[Token] = tokens.headOption

  lazy val lastTokenOption: Option[Token] = tokens.lastOption

  def firstToken = firstTokenOption.get

  lazy val lastToken = lastTokenOption.get
  
  //for CompilationUnit it will be null
  var enclosingTopLevelClass: TypeDefSymbol = null

  protected trait Flattenable {
    def tokens: IList[Token]
  }
  
  def getAllChildrenInclude: IList[JawaAstNode] = {
    this :: getAllChildren
  }
  
  def getAllChildren: IList[JawaAstNode] = {
    val allAsts: MList[JawaAstNode] = mlistEmpty
    val worklist: MList[JawaAstNode] = mlistEmpty
    allAsts += this
    allAsts ++= this.immediateChildren
    worklist ++= this.immediateChildren
    while(!worklist.isEmpty){
      val node = worklist.remove(0)
      allAsts ++= node.immediateChildren
      worklist ++= node.immediateChildren
    }
    allAsts.toList
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

  def toCode: String = {
    val sb: StringBuilder = new StringBuilder
    val (startline, startcolumn) = firstTokenOption match {
      case Some(ft) => (ft.line, ft.column)
      case None => (0, 0)
    }
    var prevline: Int = 0
    var prevcolumn: Int = 0
    tokens.foreach {
      token =>
        val line = token.line - startline
        val column = if(token.line == 0) token.column - startcolumn else token.column
        if(line != prevline) prevcolumn = 0
        val text = token.rawtext
        for(i <- 1 to line - prevline){
          sb.append("\n")
        }
        for(i <- 1 to column - prevcolumn){
          sb.append(" ")
        }
        prevline = line
        prevcolumn = column + token.length
        sb.append(text)
    }
    sb.toString.trim
  }
  
  /**
   * Returns range of tokens in the node, or None if there are no tokens in the node
   */
  def rangeOpt: Option[Range] =
    if (tokens.isEmpty)
      None
    else {
      val firstIndex = tokens.head.pos.start
      val lastIndex = tokens.last.lastCharacterOffset
      Some(Range(firstIndex, lastIndex - firstIndex + 1))
    }

  def pos: Position = {
    if(tokens.isEmpty) NoPosition
    else {
      val firstIndex = tokens.head.pos.start
      val lastIndex = tokens.last.lastCharacterOffset
      Position.range(firstToken.file, firstIndex, lastIndex - firstIndex + 1)
    }
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

sealed trait JawaSymbol extends JawaAstNode {
  def id: Token
}

sealed trait DefSymbol extends JawaSymbol

sealed trait RefSymbol extends JawaSymbol

sealed trait ClassSym {
  def typ: ObjectType
}
sealed trait MethodSym {
  def signature: Signature
}
sealed trait FieldSym{
  def FQN: String
}
sealed trait VarSym{
  def varName: String
  def owner: MethodDeclaration
}
sealed trait LocationSym{
  def location: String
  def owner: MethodDeclaration
}

case class TypeDefSymbol(id: Token) extends DefSymbol with ClassSym {
  lazy val tokens = flatten(id)
  def typ: ObjectType = getTypeFromName(id.text).asInstanceOf[ObjectType]
}

case class TypeSymbol(id: Token) extends RefSymbol with ClassSym {
  lazy val tokens = flatten(id)
  def typ: ObjectType = getTypeFromName(id.text).asInstanceOf[ObjectType]
}

case class MethodDefSymbol(id: Token) extends DefSymbol with MethodSym {
  lazy val tokens = flatten(id)
  def baseType: ObjectType = getClassTypeFromMethodFullName(id.text)
  var signature: Signature = null
  def methodName: String = getMethodNameFromMethodFullName(id.text)
}

case class MethodNameSymbol(id: Token) extends RefSymbol with MethodSym {
  lazy val tokens = flatten(id)
  def baseType: ObjectType = getClassTypeFromMethodFullName(id.text)
  var signature: Signature = null
  def methodName: String = getMethodNameFromMethodFullName(id.text)
}

case class FieldDefSymbol(id: Token) extends DefSymbol with FieldSym {
  lazy val tokens = flatten(id)
  def FQN: String = id.text.replaceAll("@@", "")
  def baseType: ObjectType = getClassTypeFromFieldFQN(FQN)
  def fieldName: String = getFieldNameFromFieldFQN(FQN)
}

case class FieldNameSymbol(id: Token) extends RefSymbol {
  lazy val tokens = flatten(id)
  def FQN: String = id.text.replaceAll("@@", "")
  def baseType: ObjectType = getClassTypeFromFieldFQN(FQN)
  def fieldName: String = getFieldNameFromFieldFQN(FQN)
}

case class SignatureSymbol(id: Token) extends RefSymbol with MethodSym {
  lazy val tokens = flatten(id)
  def signature: Signature = Signature(id.text)
//  def FQMN: String = signature.FQMN
  def methodName: String = signature.methodNamePart
}

case class VarDefSymbol(id: Token) extends DefSymbol with VarSym {
  lazy val tokens = flatten(id)
  def varName: String = id.text
  var owner: MethodDeclaration = null
}

case class VarSymbol(id: Token) extends RefSymbol with VarSym {
  lazy val tokens = flatten(id)
  def varName: String = id.text
  var owner: MethodDeclaration = null
}

/**
 * LocationSymbol is following form: #L00001.
 */
case class LocationDefSymbol(id: Token) extends DefSymbol with LocationSym {
  lazy val tokens = flatten(id)
  def location: String = id.text.substring(1, id.text.length() - 1)
  var owner: MethodDeclaration = null
}

/**
 * JumpLocationSymbol is following form: L00001
 */
case class LocationSymbol(id: Token) extends RefSymbol with LocationSym {
  lazy val tokens = flatten(id)
  def location: String = id.text
  var owner: MethodDeclaration = null
}

case class ClassOrInterfaceDeclaration(
    dclToken: Token, 
    cityp: TypeDefSymbol,
    annotations: IList[Annotation], 
    extendsAndImplimentsClausesOpt: Option[ExtendsAndImplimentsClauses],
    instanceFieldDeclarationBlock: InstanceFieldDeclarationBlock,
    staticFields: IList[StaticFieldDeclaration],
    methods: IList[MethodDeclaration]) extends Declaration with ParsableAstNode {
  lazy val tokens = flatten(dclToken, cityp, annotations, extendsAndImplimentsClausesOpt, instanceFieldDeclarationBlock, staticFields, methods)
  def isInterface: Boolean = {
    annotations.exists { a => a.key == "type" && a.value == "interface" }
  }
  def parents: IList[ObjectType] = extendsAndImplimentsClausesOpt match {case Some(e) => e.parents; case None => ilistEmpty}
  def superClassOpt: Option[ObjectType] = extendsAndImplimentsClausesOpt match{case Some(e) => e.superClassOpt; case None => None}
  def interfaces: IList[ObjectType] = extendsAndImplimentsClausesOpt match {case Some(e) => e.interfaces; case None => ilistEmpty}
  def fields: IList[Field with Declaration] = instanceFieldDeclarationBlock.instanceFields ++ staticFields
  def instanceFields: IList[InstanceFieldDeclaration] = instanceFieldDeclarationBlock.instanceFields
  def typ: ObjectType = cityp.typ.asInstanceOf[ObjectType]
}

case class Annotation(
    at: Token, 
    annotationID: Token, 
    annotationValueOpt: Option[Either[JawaSymbol, Token]]) extends JawaAstNode {
  lazy val tokens = flatten(at, annotationID, annotationValueOpt)
  def key: String = annotationID.text
  def value: String = 
    annotationValueOpt match{
      case Some(Left(a)) => a.id.text 
      case Some(Right(a)) => a.text
      case None => ""
    }
}

case class ExtendsAndImplimentsClauses(
    extendsAndImplementsToken: Token,
    parentTyps: IList[(ExtendAndImpliment, Option[Token])]) extends JawaAstNode {
  require(parentTyps.filter(_._1.isExtend).size <= 1)
  lazy val tokens = flatten(extendsAndImplementsToken, parentTyps)
  def parents: IList[ObjectType] = parentTyps.map(_._1.typ.asInstanceOf[ObjectType])
  def superClassOpt: Option[ObjectType] = parentTyps.find(_._1.isExtend).map(_._1.typ)
  def interfaces: IList[ObjectType] = parentTyps.filter(_._1.isImplement).map(_._1.typ)
}

case class ExtendAndImpliment(
    parenttyp: TypeSymbol,
    annotations: IList[Annotation])extends JawaAstNode {
  lazy val tokens = flatten(parenttyp, annotations)
  def typ: ObjectType = parenttyp.typ
  def isExtend: Boolean = annotations.exists { a => a.key == "type" && a.value == "class" }
  def isImplement: Boolean = annotations.exists { a => a.key == "type" && a.value == "interface" }
}

sealed trait Field extends JawaAstNode {
  def typ: Type
  def fieldSymbol: FieldDefSymbol
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
    fieldSymbol: FieldDefSymbol,
    annotations: IList[Annotation], 
    semi: Token) extends Field with Declaration {
  lazy val tokens = flatten(typ, fieldSymbol, annotations, semi)
  def FQN: String = fieldSymbol.FQN
  def isStatic: Boolean = false
}

case class StaticFieldDeclaration(
    staticFieldToken: Token, 
    typ: Type, 
    fieldSymbol: FieldDefSymbol,
    annotations: IList[Annotation], 
    semi: Token) extends Field with Declaration {
  lazy val tokens = flatten(staticFieldToken, typ, fieldSymbol, annotations, semi)
  def FQN: String = fieldSymbol.FQN
  def isStatic: Boolean = true
}

case class Type(base: Either[TypeSymbol, Token], typeFragments: IList[TypeFragment]) extends JawaAstNode {
  lazy val tokens = flatten(base, typeFragments)
  def dimentions: Int = typeFragments.size
  def baseType: JawaType = 
    base match {
      case Left(ts) => ts.typ
      case Right(t) => getTypeFromName(t.text)
    }
  def typ: JawaType = getType(baseType.typ, dimentions)
}

sealed trait TypeFragment extends JawaAstNode

case class RawTypeFragment(lbracket: Token, rbracket: Token) extends TypeFragment {
  lazy val tokens = flatten(lbracket, rbracket)
}

case class TypeFragmentWithInit(lbracket: Token, varSymbols: IList[(VarSymbol, Option[Token])], rbracket: Token) extends TypeFragment {
  lazy val tokens = flatten(lbracket, varSymbols, rbracket)
}

case class MethodDeclaration(
    dclToken: Token,
    returnType: Type,
    methodSymbol: MethodDefSymbol,
    paramClause: ParamClause,
    annotations: IList[Annotation],
    var body: Body) extends Declaration with ParsableAstNode {
  lazy val tokens = flatten(dclToken, returnType, methodSymbol, paramClause, annotations, body)
  def isConstructor: Boolean = isJawaConstructor(methodSymbol.id.text)
  def name: String = methodSymbol.id.text.substring(methodSymbol.id.text.lastIndexOf(".") + 1)
  def owner: String = annotations.find { a => a.key == "owner" }.get.value
  def signature: Signature = Signature(annotations.find { a => a.key == "signature" }.get.value)
  def thisParam: Option[Param] = paramClause.thisParam
  def param(i: Int): Param = paramClause.param(i)
  def paramlist: IList[Param] = paramClause.paramlist
}

case class ParamClause(
    lparen: Token,
    params: IList[(Param, Option[Token])], 
    rparen: Token) extends JawaAstNode {
  lazy val tokens = flatten(lparen, params, rparen)
  def thisParam: Option[Param] = params.find { x => x._1.isThis }.map(_._1)
  def param(i: Int): Param =
    i match {
      case n if (n >= 0 && n < paramlist.size) => paramlist(n)
      case _ => throw new IndexOutOfBoundsException("List size " + paramlist.size + " but index " + i)
    }
  def paramlist: IList[Param] = params.filterNot(_._1.isThis).map(_._1)
}

case class Param(
    typ: Type, 
    paramSymbol: VarDefSymbol, 
    annotations: IList[Annotation]) extends JawaAstNode {
  lazy val tokens = flatten(typ, paramSymbol, annotations)
  def isThis: Boolean = annotations.exists { a => a.key == "type" && a.value == "this" }
  def isObject: Boolean = annotations.exists { a => a.key == "type" && (a.value == "this" || a.value == "object") }
  def name: String = paramSymbol.id.text
}

sealed trait Body extends ParsableAstNode

case class UnresolvedBody(bodytokens: IList[Token]) extends Body {
  lazy val tokens = flatten(bodytokens)
}

case class ResolvedBody(
    lbrace: Token, 
    locals: IList[LocalVarDeclaration], 
    locations: IList[Location], 
    catchClauses: IList[CatchClause], 
    rbrace: Token) extends Body {
  lazy val tokens = flatten(lbrace, locals, locations, catchClauses, rbrace)
}

case class LocalVarDeclaration(
    varSymbol: VarDefSymbol, 
    annotations: IList[Annotation], 
    semi: Token) extends Declaration {
  lazy val tokens = flatten(varSymbol, annotations, semi)
}

case class Location(
    locationSymbol: LocationDefSymbol, 
    statement: Statement, 
    semiOpt: Option[Token]) extends ParsableAstNode {
  lazy val tokens = flatten(locationSymbol, statement, semiOpt)
  def locationUri: String = {
    if(locationSymbol.id.length <= 1) ""
    else locationSymbol.location
  }
  var locationIndex: Int = 0
}

sealed trait Statement extends JawaAstNode

case class CallStatement(
    callToken: Token, 
    lhs: VarSymbol, 
    assignOP: Token,
    methodNameSymbol: MethodNameSymbol,
    argClause: ArgClause,
    annotations: IList[Annotation]) extends Statement {
  lazy val tokens = flatten(callToken, lhs, assignOP, methodNameSymbol, argClause, annotations)
  //default is virtual call
  def typ: String = annotations.find { a => a.key == "type" }.map(_.value).getOrElse("virtual")
  def signature: Signature = Signature(annotations.find { a => a.key == "signature" }.get.value)
  def classDescriptor: String = annotations.find { a => a.key == "classDescriptor" }.get.value
  def isStatic: Boolean = typ == "static"
  def isVirtual: Boolean = typ == "virtual"
  def isSuper: Boolean = typ == "super"
  def isDirect: Boolean = typ == "direct"
  def recvOpt: Option[String] = if(isStatic) None else Some(argClause.arg(0))
  def args: IList[String] = if(isStatic) argClause.varSymbols.map(_._1.id.text) else argClause.varSymbols.tail.map(_._1.id.text)
  def arg(i: Int): String = {
    i match {
      case n if (n >= 0 && n < args.size) => args(n)
      case _ => throw new IndexOutOfBoundsException("List size " + args.size + " but index " + i)
    }
  }
}

case class ArgClause(
    lparen: Token, 
    varSymbols: IList[(VarSymbol, Option[Token])], 
    rparen: Token) extends JawaAstNode {
  lazy val tokens = flatten(lparen, varSymbols, rparen)
  def arg(i: Int): String =
    i match {
      case n if (n >= 0 && n < varSymbols.size) => varSymbols(n)._1.id.text
      case _ => throw new IndexOutOfBoundsException("List size " + varSymbols.size + " but index " + i)
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
    varSymbol: VarSymbol) extends Statement {
  lazy val tokens = flatten(throwToken, varSymbol)
}

case class IfStatement(
    ifToken: Token,
    exp: Expression,
    thengoto: (Token, Token),
    targetLocation: LocationSymbol) extends Statement {
  lazy val tokens = flatten(ifToken, exp, thengoto, targetLocation)
}

case class GotoStatement(
    goto: Token,
    targetLocation: LocationSymbol) extends Statement {
  lazy val tokens = flatten(goto, targetLocation)
}

case class SwitchStatement(
    switchToken: Token,
    condition: VarSymbol,
    cases: IList[SwitchCase],
    defaultCaseOpt: Option[SwitchDefaultCase]) extends Statement {
  lazy val tokens = flatten(switchToken, condition, cases, defaultCaseOpt)
}

case class SwitchCase(
    bar: Token,
    constant: Token,
    arrow: Token,
    goto: Token,
    targetLocation: LocationSymbol) extends JawaAstNode {
  lazy val tokens = flatten(bar, constant, arrow, goto, targetLocation)
}

case class SwitchDefaultCase(
    bar: Token,
    elseToken: Token,
    arrow: Token,
    goto: Token,
    targetLocation: LocationSymbol) extends JawaAstNode {
  lazy val tokens = flatten(bar, elseToken, arrow, goto, targetLocation)
}

case class ReturnStatement(
    returnToken: Token,
    varOpt: Option[VarSymbol],
    annotations: IList[Annotation]) extends Statement {
  lazy val tokens = flatten(returnToken, varOpt, annotations)
}

case class EmptyStatement(
    annotations: IList[Annotation]) extends Statement {
  lazy val tokens = flatten(annotations)
}

sealed trait Expression extends JawaAstNode

case class NameExpression(
    varSymbol: Either[VarSymbol, FieldNameSymbol], // FieldNameSymbol here is static fields
    annotations: IList[Annotation]) extends Expression {
  lazy val tokens = flatten(varSymbol, annotations)
  def name: String = 
    varSymbol match {
      case Left(v) => v.varName
      case Right(f) => f.FQN
    }
  def isStatic: Boolean = varSymbol.isRight
}

case class IndexingExpression(
    varSymbol: VarSymbol,
    indices: IList[IndexingSuffix]) extends Expression {
  lazy val tokens = flatten(varSymbol, indices)
  def dimentions: Int = indices.size
}

case class IndexingSuffix(
    lbracket: Token,
    index: Either[VarSymbol, Token],
    rbracket: Token) extends JawaAstNode {
  lazy val tokens = flatten(lbracket, index, rbracket)
}
    
case class AccessExpression(
    varSymbol: VarSymbol,
    dot: Token,
    fieldID: Token) extends Expression {
  lazy val tokens = flatten(varSymbol, dot, fieldID)
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
  unary: Either[VarSymbol, Token])
    extends Expression {
  lazy val tokens = flatten(op, unary)
}

case class BinaryExpression(
  left: Either[VarSymbol, Token],
  op: Token,
  right: Either[VarSymbol, Token])
    extends Expression {
  lazy val tokens = flatten(left, op, right)
}

case class CmpExpression(
    cmp: Token,
    lparen: Token,
    var1Symbol: VarSymbol,
    comma: Token,
    var2Symbol: VarSymbol,
    rparen: Token) extends Expression {
  lazy val tokens = flatten(cmp, lparen, var1Symbol, comma, var2Symbol, rparen)
}

case class CatchClause(
    catchToken: Token,
    typOrAny: Either[Type, Token],
    range: CatchRange,
    goto: Token,
    targetLocation: LocationSymbol,
    semi: Token) extends JawaAstNode {
  lazy val tokens = flatten(catchToken, typOrAny, range, goto, targetLocation, semi)
  def from: String = range.fromLocation.location
  def to: String = range.toLocation.location
}

case class CatchRange(
    at: Token,
    lbracket: Token,
    fromLocation: LocationSymbol,
    range: Token,
    toLocation: LocationSymbol,
    rbracket: Token) extends JawaAstNode {
  lazy val tokens = flatten(at, lbracket, fromLocation, range, toLocation, rbracket)
}