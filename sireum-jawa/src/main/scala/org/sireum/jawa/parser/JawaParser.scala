/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.parser

import org.sireum.jawa.lexer._
import org.sireum.jawa.lexer.Tokens._
import org.sireum.util._

class JawaParser(tokens: Array[Token]) {
  private val logging: Boolean = false

//  private val forgiving: Boolean = true

  import JawaParser._

  def safeParse[T](production: ⇒ T): Option[T] = try Some(production) catch { case e: JawaParserException ⇒ None }

  require(!tokens.isEmpty) // at least EOF
  
  def compilationUnit(): CompilationUnit = {
    val topDecls: MList[ClassOrInterfaceDeclaration] = mlistEmpty
    def loop() {
      currentTokenType match {
        case CLASS_OR_INTERFACE =>
          topDecls += classOrInterfaceDeclaration()
          loop()
        case _ =>
      }
    }
    loop()
    val eofToken = accept(EOF)
    CompilationUnit(topDecls.toList, eofToken)
  }
  
  private def classOrInterfaceDeclaration(): ClassOrInterfaceDeclaration = {
    val dclToken: Token = accept(CLASS_OR_INTERFACE)
    val nameID: Token = accept(ID)
    val annotations_ : IList[Annotation] = annotations()
    val extendsAndImplimentsClausesOpt_ : Option[ExtendsAndImplimentsClauses] = extendsAndImplimentsClausesOpt()
    val instanceFieldDeclarationBlock_ : InstanceFieldDeclarationBlock = instanceFieldDeclarationBlock()
    val staticFields: IList[StaticFieldDeclaration] = staticFieldDeclarations()
    val methods: IList[MethodDeclaration] = methodDeclarations()
    ClassOrInterfaceDeclaration(dclToken, nameID, annotations_, extendsAndImplimentsClausesOpt_, instanceFieldDeclarationBlock_, staticFields, methods)
  }
  
  private def annotations(): IList[Annotation] = {
    val annos: MList[Annotation] = mlistEmpty
    def loop(){
      currentTokenType match {
        case AT =>
          val at = nextToken()
          val annotationID: Token = accept(ID)
          val annotationValueOpt: Option[Token] =
            currentTokenType match{
              case ID => Some(nextToken())
              case x if isLiteralToken(x) => Some(nextToken())
              case _ => None
            }
          annos += Annotation(at, annotationID, annotationValueOpt)
          loop()
        case _ =>
      }
    }
    loop()
    annos.toList
  }
  
  private def extendsAndImplimentsClausesOpt(): Option[ExtendsAndImplimentsClauses] = {
    currentTokenType match {
      case EXTENDS_AND_IMPLEMENTS =>
        val extendsAndImplementsToken: Token = accept(EXTENDS_AND_IMPLEMENTS)
        val firstParentID: Token = accept(ID)
        val restparentIDs: MList[(Token, Token)] = mlistEmpty
        def loop() {
          currentTokenType match {
            case COMMA =>
              val comma = nextToken()
              val parentID = accept(ID)
              restparentIDs += ((comma, parentID))
              loop()
            case _ =>
          }
        }
        loop()
        Some(ExtendsAndImplimentsClauses(extendsAndImplementsToken, firstParentID, restparentIDs.toList))
      case _ => None
    }
  }
  
  private def instanceFieldDeclarationBlock(): InstanceFieldDeclarationBlock = {
    val lbrace: Token = accept(LBRACE)
    val instanceFields: IList[InstanceFieldDeclaration] = instanceFieldDeclarations()
    val rbrace: Token = accept(RBRACE)
    InstanceFieldDeclarationBlock(lbrace, instanceFields, rbrace)
  }
  
  private def instanceFieldDeclarations(): IList[InstanceFieldDeclaration] = {
    val instanceFields: MList[InstanceFieldDeclaration] = mlistEmpty
    while(currentTokenType != RBRACE){
      val typ_ : Type = typ()
      val nameID: Token = accept(ID)
      val annotations_ = annotations()
      val semi: Token = accept(SEMI)
      instanceFields += InstanceFieldDeclaration(typ_, nameID, annotations_, semi)
    }
    instanceFields.toList
  }
  
  private def staticFieldDeclarations(): IList[StaticFieldDeclaration] = {
    val staticFields: MList[StaticFieldDeclaration] = mlistEmpty
    def loop(){
      currentTokenType match {
        case STATIC_FIELD =>
          val staticFieldToken: Token = accept(STATIC_FIELD)
          val typ_ : Type = typ()
          val nameID: Token = accept(STATIC_ID)
          val annotations_ = annotations()
          val semi: Token = accept(SEMI)
          staticFields += StaticFieldDeclaration(staticFieldToken, typ_, nameID, annotations_, semi)
          loop()
        case _ =>
      }
    }
    loop()
    staticFields.toList
  }
  
  private def methodDeclarations(): IList[MethodDeclaration] = {
    val methods: MList[MethodDeclaration] = mlistEmpty
    def loop() {
      currentTokenType match {
        case METHOD =>
          val dclToken: Token = nextToken()
          val returnType: Type = typ()
          val nameID: Token = accept(ID)
          val paramClause_ : ParamClause = paramClause()
          val annotations_ : IList[Annotation] = annotations()
          val body_ : Body = body()
          methods += MethodDeclaration(dclToken, returnType, nameID, paramClause_, annotations_, body_)
          loop()
        case _ =>
      }
    }
    loop()
    methods.toList
  }
  
  private def paramClause(): ParamClause = {
    val lparen = accept(LPAREN)
    val params: MList[(Param, Option[Token])] = mlistEmpty
    while(currentTokenType != RPAREN) {
      val param_ : Param = param()
      val commaOpt: Option[Token] = 
        currentTokenType match {
        case COMMA => Some(nextToken())
        case _ => None
      }
      params += ((param_, commaOpt))
    }
    val rparen = accept(RPAREN)
    ParamClause(lparen, params.toList, rparen)
  }
  
  private def param(): Param = {
    val typ_ : Type = typ()
    val nameID: Token = accept(ID)
    val annotations_ = annotations()
    Param(typ_, nameID, annotations)
  }
  
  private def body(): Body = {
    val lbrace: Token = accept(LBRACE)
    val locals: IList[LocalVarDeclaration] = localVarDeclarations()
    val locations_ : IList[Location] = locations()
    val catchClauses_ : IList[CatchClause] = catchClauses()
    val rbrace: Token = accept(RBRACE)
    Body(lbrace, locals, locations_, catchClauses_, rbrace)
  }
  
  private def localVarDeclarations(): IList[LocalVarDeclaration] = {
    val locals: MList[LocalVarDeclaration] = mlistEmpty
    while(currentTokenType != LOCATION_ID){
      val nameID: Token = accept(ID)
      val annotations_ : IList[Annotation] = annotations()
      val semi: Token = accept(SEMI)
      locals += LocalVarDeclaration(nameID, annotations_, semi)
    }
    locals.toList
  }
  
  private def locations(): IList[Location] = {
    val locations: MList[Location] = mlistEmpty
    def loop(){
      currentTokenType match {
        case LOCATION_ID =>
          locations += location0()
          loop()
        case _ =>
      }
    }
    loop()
    locations.toList
  }
  
  def location(): Location = location0()
  
  private def location0(): Location = {
    val locationID = nextToken
    val statement_ : Statement = statement()
    val semiOpt: Option[Token] = 
      currentTokenType match {
        case SEMI => Some(nextToken())
        case _ => None
      }
    Location(locationID, statement_, semiOpt)
  }
  
  private def statement(): Statement = {
    currentTokenType match {
      case CALL => callStatement()
      case THROW => throwStatement()
      case IF => ifStatement()
      case SWITCH => switchStatement()
      case RETURN => returnStatement()
      case GOTO => gotoStatement()
      case AT | SEMI | LOCATION_ID => emptyStatement()
      case _ => assignmentStatement()
    }
  }
  
  private def callStatement(): CallStatement = {
    val callToken: Token = accept(CALL)
    val lhs: Expression = expression()
    val assignmentOP: Token = accept(ASSIGN_OP)
    val invokeID: Token = accept(ID)
    val argClause_ : ArgClause = argClause()
    val annotations_ : IList[Annotation] = annotations()
    CallStatement(callToken, lhs, assignmentOP, invokeID, argClause_, annotations_)
  }
  
  private def argClause(): ArgClause = {
    val lparen: Token = accept(LPAREN)
    val varIDs: MList[(Token, Option[Token])] = mlistEmpty
    while(currentTokenType != RPAREN) {
      val varID: Token = accept(ID)
      val commaOpt: Option[Token] =
        currentTokenType match {
          case COMMA => Some(nextToken())
          case _ => None
        }
      varIDs += ((varID, commaOpt))
    }
    val rparen: Token = accept(RPAREN)
    ArgClause(lparen, varIDs.toList, rparen)
  }
  
  private def assignmentStatement(): AssignmentStatement = {
    val lhs: Expression = expression()
    val assignOP: Token = accept(ASSIGN_OP)
    val rhs: Expression = expression()
    val annotations_ : IList[Annotation] = annotations()
    AssignmentStatement(lhs, assignOP, rhs, annotations_)
  }
  
  private def throwStatement(): ThrowStatement = {
    val throwToken: Token = accept(THROW)
    val variableID: Token = accept(ID)
    ThrowStatement(throwToken, variableID)
  }
  
  private def ifStatement(): IfStatement = {
    val ifToken: Token = accept(IF)
    val exp: Expression = expression()
    val thengoto: (Token, Token) = (accept(THEN), accept(GOTO))
    val targetLocation: Token = accept(ID)
    IfStatement(ifToken, exp, thengoto, targetLocation)
  }
  
  private def gotoStatement(): GotoStatement = {
    val goto: Token = accept(GOTO)
    val targetLocation: Token = accept(ID)
    GotoStatement(goto, targetLocation)
  }
  
  private def switchStatement(): SwitchStatement = {
    val switchToken: Token = accept(SWITCH)
    val condition: Token = accept(ID)
    val cases: IList[SwitchCase] = switchCases()
    val defaultCaseOpt: Option[SwitchDefaultCase] = switchDefaultCaseOpt()
    SwitchStatement(switchToken, condition, cases, defaultCaseOpt)
  }
  
  private def switchCases(): IList[SwitchCase] = {
    val cases: MList[SwitchCase] = mlistEmpty
    def loop(){
      val next = lookahead(1)
      if(next == INTEGER_LITERAL){
        currentTokenType match {
          case OP =>
            val bar: Token = accept(OP)
            if(bar.text != "|") throw new JawaParserException("Expected op token " + "'|'" + " but got " + currentToken)
            val constant: Token = accept(INTEGER_LITERAL)
            val arrow: Token = accept(ARROW)
            val goto: Token = accept(GOTO)
            val targetLocation: Token = accept(ID)
            cases += SwitchCase(bar, constant, arrow, goto, targetLocation)
            loop()
          case _ =>
        }
      }
    }
    loop()
    cases.toList
  }
  
  private def switchDefaultCaseOpt(): Option[SwitchDefaultCase] = {
    currentTokenType match {
      case OP =>
        if(currentToken.text == "|"){
          val bar: Token = nextToken()
          val elseToken: Token = accept(ELSE)
          val arrow: Token = accept(ARROW)
          val goto: Token = accept(GOTO)
          val targetLocation: Token = accept(ID)
          Some(SwitchDefaultCase(bar, elseToken, arrow, goto, targetLocation))
        } else None
      case _ => None
    }
  }
  
  private def returnStatement(): ReturnStatement = {
    val returnToken: Token = accept(RETURN)
    val expOpt: Option[Expression] = expressionOpt()
    val annotations_ : IList[Annotation] = annotations()
    ReturnStatement(returnToken, expOpt, annotations_)
  }
  
  private def emptyStatement(): EmptyStatement = {
    val annotations_ : IList[Annotation] = annotations()
    EmptyStatement(annotations_)
  }
  
  private def expressionOpt(): Option[Expression] = {
    try{
      Some(expression())
    } catch {
      case jpe: JawaParserException => None
      case e: Throwable => throw e
    }
  }
  
  private def expression(): Expression = {
    currentTokenType match {
      case NEW => newExpression()
      case LPAREN => castExpression()
      case x if isLiteralToken(currentTokenType) => literalExpression()
      case x if isUnaryOP(currentToken) => unaryExpression()
      case STATIC_ID =>
        nameExpression()
      case ID =>
        val next: TokenType = lookahead(1)
        next match {
          case DOT => accessExpression()
          case LBRACKET => indexingExpression()
          case OP => binaryExpression()
          case _ => nameExpression()
        }
      case _ => throw new JawaParserException("Unexpected expression start: " + currentToken)
    }
  }
  
  private def nameExpression(): NameExpression = {
    if(currentTokenType != ID && currentTokenType != STATIC_ID)
      throw new JawaParserException("expected 'ID' or 'STATIC_ID' but " + currentToken + " found")
    val nameID: Token = nextToken()
    val annotations_ : IList[Annotation] = annotations()
    NameExpression(nameID, annotations_)
  }
  
  private def indexingExpression(): IndexingExpression = {
    val baseID: Token = accept(ID)
    val indices: IList[IndexingSuffix] = indexingSuffixs()
    IndexingExpression(baseID, indices)
  }
  
  private def indexingSuffixs(): IList[IndexingSuffix] = {
    val indices: MList[IndexingSuffix] = mlistEmpty
    def loop(){
      currentTokenType match {
        case LBRACKET =>
          val lbracket: Token = nextToken()
          val exp: Expression = expression()
          val rbracket: Token = accept(RBRACKET)
          indices += IndexingSuffix(lbracket, exp, rbracket)
          loop()
        case _ =>
      }
    }
    loop()
    indices.toList
  }
  
  private def accessExpression(): AccessExpression = {
    val baseID: Token = accept(ID)
    val dot: Token = accept(DOT)
    val fieldID: Token = accept(ID)
    AccessExpression(baseID, dot, fieldID)
  }
  
  private def castExpression(): CastExpression = {
    val lparen: Token = accept(LPAREN)
    val typ_ : Type = typ()
    val rparen: Token = accept(RPAREN)
    val exp: Expression = expression()
    CastExpression(lparen, typ_, rparen, exp)
  }
  
  private def newExpression(): NewExpression = {
    val newToken: Token = accept(NEW)
    val typ_ : Type = typ()
    NewExpression(newToken, typ_)
  }
  
  private def literalExpression(): LiteralExpression = {
    if(!isLiteral) throw new JawaParserException("expected literal but found " + currentToken)
    val constant: Token = nextToken()
    LiteralExpression(constant)
  }
  
  private def unaryExpression(): UnaryExpression = {
    val op: Token = accept(OP) // need to check is it unary op
    val exp: Expression = expression()
    UnaryExpression(op, exp)
  }
  
  private def binaryExpression(): BinaryExpression = {
    if(currentTokenType != ID && 
       currentTokenType != INTEGER_LITERAL &&
       currentTokenType != FLOATING_POINT_LITERAL)
      throw new JawaParserException("expected 'ID' or 'INTEGER_LITERAL' or 'FLOATING_POINT_LITERAL' but " + currentToken + " found")
    val left: Token = nextToken()
    val op: Token = accept(OP) // need to check is it binary op
    if(currentTokenType != ID && 
       currentTokenType != INTEGER_LITERAL &&
       currentTokenType != FLOATING_POINT_LITERAL)
      throw new JawaParserException("expected 'ID' or 'INTEGER_LITERAL' or 'FLOATING_POINT_LITERAL' but " + currentToken + " found")
    val right: Token = nextToken()
    BinaryExpression(left, op, right)
  }
  
  private def catchClauses(): IList[CatchClause] = {
    val catchClauses: MList[CatchClause] = mlistEmpty
    def loop() {
      currentTokenType match {
        case CATCH =>
          catchClauses += catchClause()
          loop()
        case _ =>
      }
    }
    loop()
    catchClauses.toList
  }
  
  private def catchClause(): CatchClause = {
    val catchToken: Token = accept(CATCH)
    val typ_ : Type = typ()
    val range: CatchRange = catchRange()
    val goto: Token = accept(GOTO)
    val targetLocation: Token = accept(ID)
    val semi: Token = accept(SEMI)
    CatchClause(catchToken, typ_, range, goto, targetLocation, semi)
  }
  
  private def catchRange(): CatchRange = {
    val at: Token = accept(AT)
    val lbracket: Token = accept(LBRACKET)
    val fromLocation: Token = accept(ID)
    val range: Token = accept(RANGE)
    val toLocation: Token = accept(ID)
    val rbracket: Token = accept(RBRACKET)
    CatchRange(at, lbracket, fromLocation, range, toLocation, rbracket)
  }
  
  private def typ(): Type = {
    val baseTypeID: Token = accept(ID)
    val typeFragments: MList[TypeFragment] = mlistEmpty
    def loop() {
      currentTokenType match {
        case LBRACKET =>
          val lbracket: Token = nextToken()
          val varIDOpt: Option[Token] = 
            currentTokenType match {
              case ID => Some(nextToken())
              case _ => None
            } 
          val rbracket: Token = accept(RBRACKET)
          typeFragments += TypeFragment(lbracket, varIDOpt, rbracket)
          loop()
        case _ =>
      }
    }
    loop()
    Type(baseTypeID, typeFragments.toList)
  }
  
  private def isUnaryOP(token: Token): Boolean = {
    val text = token.text
    text match {
      case "+" | "-" | "/" | "%" | "*" | "!" | "~" => true
      case _ => false
    }
  }
  
  private def isLiteralToken(tokenType: TokenType): Boolean = LITERALS.contains(tokenType)

  private def isLiteral = isLiteralToken(currentTokenType)
  
  private def accept(tokenType: TokenType): Token =
    if (currentTokenType == tokenType)
      nextToken()
    else
      throw new JawaParserException("Expected token " + tokenType + " but got " + currentToken)

  private var tokensArray: Array[Token] = tokens.toArray

  private var pos = 0

  private def currentToken: Token = this(pos)

  private def apply(pos: Int): Token =
    if (pos < tokensArray.length)
      tokensArray(pos)
    else
      tokens.last

  private def currentTokenType = currentToken.tokenType

  /** @return the token before advancing */
  private def nextToken(): Token = {
    val token = currentToken
    pos += 1
    if (logging)
      println("nextToken(): " + token + " --> " + currentToken)
    token
  }

  private def lookahead(n: Int): TokenType = this(pos + n).tokenType

  private def log[T](s: String)(f: ⇒ T): T = {
    if (logging) {
      println("Enter " + s + " [" + currentToken + "]")
      val result = f
      println("Exit " + s + " [" + currentToken + "]")
      result
    } else
      f
  }
  
}

object JawaParser {

  /**
   * Parse the given source as a compilation unit
   * @return None if there is a parse error.
   */
  def parse(source: Either[String, ResourceUri]): Option[JawaAstNode] = {
    val parser = new JawaParser(JawaLexer.tokenise(source).toArray)
    parser.safeParse(parser.compilationUnit())
  }
}