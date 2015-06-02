/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.parser

import org.sireum.jawa.sjc.lexer._
import org.sireum.jawa.sjc.lexer.Tokens._
import org.sireum.util._
import org.sireum.jawa.sjc.io.AbstractFile
import org.sireum.jawa.sjc.Reporter
import org.sireum.jawa.sjc.ObjectType

class JawaParser(tokens: Array[Token], reporter: Reporter) {
  private val logging: Boolean = false

  import JawaParser._

  def safeParse[T <: ParsableAstNode](production: ⇒ T): Option[T] = try Some(production) catch { case e: JawaParserException ⇒ None }

  require(!tokens.isEmpty) // at least EOF
  
  def compilationUnit(resolveBody: Boolean): CompilationUnit = {
    val topDecls: MList[ClassOrInterfaceDeclaration] = mlistEmpty
    def loop() {
      currentTokenType match {
        case CLASS_OR_INTERFACE =>
          topDecls += classOrInterfaceDeclaration0(resolveBody)
          loop()
        case _ =>
      }
    }
    loop()
    val eofToken = accept(EOF)
    val cu = CompilationUnit(topDecls.toList, eofToken)
    cu.topDecls.par foreach {
      cid =>
        val typ = cid.typ
        cid.getAllChildrenInclude foreach (_.enclosingTopLevelClass = typ)
    }
    cu
  }
  
  def classOrInterfaceDeclaration(resolveBody: Boolean): ClassOrInterfaceDeclaration = classOrInterfaceDeclaration0(resolveBody)
  
  private def classOrInterfaceDeclaration0(resolveBody: Boolean): ClassOrInterfaceDeclaration = {
    val dclToken: Token = accept(CLASS_OR_INTERFACE)
    val cityp: Type = typ(withinit = false)
    val annotations_ : IList[Annotation] = annotations()
    val extendsAndImplimentsClausesOpt_ : Option[ExtendsAndImplimentsClauses] = extendsAndImplimentsClausesOpt()
    val instanceFieldDeclarationBlock_ : InstanceFieldDeclarationBlock = instanceFieldDeclarationBlock()
    val staticFields: IList[StaticFieldDeclaration] = staticFieldDeclarations()
    val methods: IList[MethodDeclaration] = methodDeclarations(resolveBody)
    ClassOrInterfaceDeclaration(dclToken, cityp, annotations_, extendsAndImplimentsClausesOpt_, instanceFieldDeclarationBlock_, staticFields, methods)
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
          val anno = Annotation(at, annotationID, annotationValueOpt)
          annos += anno
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
        val parents: MList[(Type, Option[Token])] = mlistEmpty
        def loop() {
          val parent : Type = typ(withinit = false)
          currentTokenType match {
            case COMMA =>
              val comma: Token = nextToken()
              parents += ((parent, Some(comma)))
              loop()
            case _ =>
              parents += ((parent, None))
          }
        }
        loop()
        val eic = ExtendsAndImplimentsClauses(extendsAndImplementsToken, parents.toList)
        Some(eic)
      case _ => None
    }
  }
  
  private def instanceFieldDeclarationBlock(): InstanceFieldDeclarationBlock = {
    val lbrace: Token = accept(LBRACE)
    val instanceFields: IList[InstanceFieldDeclaration] = instanceFieldDeclarations()
    val rbrace: Token = accept(RBRACE)
    val ifd = InstanceFieldDeclarationBlock(lbrace, instanceFields, rbrace)
    ifd
  }
  
  private def instanceFieldDeclarations(): IList[InstanceFieldDeclaration] = {
    val instanceFields: MList[InstanceFieldDeclaration] = mlistEmpty
    while(currentTokenType != RBRACE){
      val typ_ : Type = typ(withinit = false)
      val nameID: Token = accept(ID)
      val annotations_ = annotations()
      val semi: Token = accept(SEMI)
      val insf = InstanceFieldDeclaration(typ_, nameID, annotations_, semi)
      instanceFields += insf
    }
    instanceFields.toList
  }
  
  private def staticFieldDeclarations(): IList[StaticFieldDeclaration] = {
    val staticFields: MList[StaticFieldDeclaration] = mlistEmpty
    def loop(){
      currentTokenType match {
        case STATIC_FIELD =>
          val staticFieldToken: Token = accept(STATIC_FIELD)
          val typ_ : Type = typ(withinit = false)
          val nameID: Token = accept(STATIC_ID)
          val annotations_ = annotations()
          val semi: Token = accept(SEMI)
          val sfd = StaticFieldDeclaration(staticFieldToken, typ_, nameID, annotations_, semi)
          staticFields += sfd
          loop()
        case _ =>
      }
    }
    loop()
    staticFields.toList
  }
  
  private def methodDeclarations(resolveBody: Boolean): IList[MethodDeclaration] = {
    val methods: MList[MethodDeclaration] = mlistEmpty
    def loop() {
      currentTokenType match {
        case METHOD =>
          val method = methodDeclaration0(resolveBody)
          methods += method
          loop()
        case _ =>
      }
    }
    loop()
    methods.toList
  }
  
  def methodDeclaration(resolveBody: Boolean): MethodDeclaration = methodDeclaration0(resolveBody)
  
  private def methodDeclaration0(resolveBody: Boolean): MethodDeclaration = {
    val dclToken: Token = accept(METHOD)
    val returnType: Type = typ(withinit = false)
    val nameID: Token = accept(ID)
    val paramClause_ : ParamClause = paramClause()
    val annotations_ : IList[Annotation] = annotations()
    val body_ : Body = body0(resolveBody)
    val md = MethodDeclaration(dclToken, returnType, nameID, paramClause_, annotations_, body_)
    md
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
    val pc = ParamClause(lparen, params.toList, rparen)
    pc
  }
  
  private def param(): Param = {
    val typ_ : Type = typ(withinit = false)
    val nameID: Token = accept(ID)
    val annotations_ = annotations()
    val p = Param(typ_, nameID, annotations_)
    p
  }
  
  def body(resolveBody: Boolean): Body = body0(resolveBody)
  
  private def body0(resolveBody: Boolean): Body = {
    if(resolveBody){
      val lbrace: Token = accept(LBRACE)
      val locals: IList[LocalVarDeclaration] = localVarDeclarations()
      val locations_ : IList[Location] = locations()
      val catchClauses_ : IList[CatchClause] = catchClauses()
      val rbrace: Token = accept(RBRACE)
      val rb = ResolvedBody(lbrace, locals, locations_, catchClauses_, rbrace)
      rb
    } else {
      val bodytokens: MList[Token] = mlistEmpty
      var stop: Boolean = false
      do {
        bodytokens += nextToken()
        stop = currentTokenType == EOF || currentTokenType == METHOD || currentTokenType == CLASS_OR_INTERFACE
      } while (!stop)
      val ub = UnresolvedBody(bodytokens.toList)
      ub
    } 
  }
  
  private def localVarDeclarations(): IList[LocalVarDeclaration] = {
    val locals: MList[LocalVarDeclaration] = mlistEmpty
    while(currentTokenType != LOCATION_ID){
      val nameID: Token = accept(ID)
      val annotations_ : IList[Annotation] = annotations()
      val semi: Token = accept(SEMI)
      val local = LocalVarDeclaration(nameID, annotations_, semi)
      locals += local
    }
    locals.toList
  }
  
  private def locations(): IList[Location] = {
    val locations: MList[Location] = mlistEmpty
    def loop(){
      currentTokenType match {
        case LOCATION_ID =>
          val index = locations.size
          locations += location0(index)
          loop()
        case _ =>
      }
    }
    loop()
    locations.toList
  }
  
  def location: Location = location0()
  
  private def location0(index: Int = 0): Location = {
    val locationID = nextToken
    val statement_ : Statement = statement()
    val semiOpt: Option[Token] = 
      currentTokenType match {
        case SEMI => Some(nextToken())
        case _ => None
      }
    val l = Location(locationID, statement_, semiOpt)
    l.locationIndex = index
    l
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
    val cs = CallStatement(callToken, lhs, assignmentOP, invokeID, argClause_, annotations_)
    cs
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
    val ac = ArgClause(lparen, varIDs.toList, rparen)
    ac
  }
  
  private def assignmentStatement(): AssignmentStatement = {
    val lhs: Expression = expression()
    val assignOP: Token = accept(ASSIGN_OP)
    val rhs: Expression = expression()
    val annotations_ : IList[Annotation] = annotations()
    val as = AssignmentStatement(lhs, assignOP, rhs, annotations_)
    as
  }
  
  private def throwStatement(): ThrowStatement = {
    val throwToken: Token = accept(THROW)
    val variableID: Token = accept(ID)
    val ts = ThrowStatement(throwToken, variableID)
    ts
  }
  
  private def ifStatement(): IfStatement = {
    val ifToken: Token = accept(IF)
    val exp: Expression = expression()
    val thengoto: (Token, Token) = (accept(THEN), accept(GOTO))
    val targetLocation: Token = accept(ID)
    val is = IfStatement(ifToken, exp, thengoto, targetLocation)
    is
  }
  
  private def gotoStatement(): GotoStatement = {
    val goto: Token = accept(GOTO)
    val targetLocation: Token = accept(ID)
    val gs = GotoStatement(goto, targetLocation)
    gs
  }
  
  private def switchStatement(): SwitchStatement = {
    val switchToken: Token = accept(SWITCH)
    val condition: Token = accept(ID)
    val cases: IList[SwitchCase] = switchCases()
    val defaultCaseOpt: Option[SwitchDefaultCase] = switchDefaultCaseOpt()
    val ss = SwitchStatement(switchToken, condition, cases, defaultCaseOpt)
    ss
  }
  
  private def switchCases(): IList[SwitchCase] = {
    val cases: MList[SwitchCase] = mlistEmpty
    def loop(){
      val next = lookahead(1)
      if(next == INTEGER_LITERAL){
        currentTokenType match {
          case OP =>
            val bar: Token = accept(OP)
            if(bar.text != "|") throw new JawaParserException(currentToken.pos, "Expected op token " + "'|'" + " but got " + currentToken)
            val constant: Token = accept(INTEGER_LITERAL)
            val arrow: Token = accept(ARROW)
            val goto: Token = accept(GOTO)
            val targetLocation: Token = accept(ID)
            val cas = SwitchCase(bar, constant, arrow, goto, targetLocation)
            cases += cas
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
          val sd = SwitchDefaultCase(bar, elseToken, arrow, goto, targetLocation)
          Some(sd)
        } else None
      case _ => None
    }
  }
  
  private def returnStatement(): ReturnStatement = {
    val returnToken: Token = accept(RETURN)
    val expOpt: Option[Expression] = expressionOpt()
    val annotations_ : IList[Annotation] = annotations()
    val rs = ReturnStatement(returnToken, expOpt, annotations_)
    rs
  }
  
  private def emptyStatement(): EmptyStatement = {
    val annotations_ : IList[Annotation] = annotations()
    val es = EmptyStatement(annotations_)
    es
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
      case CMP => cmpExpression()
      case LPAREN => 
        val next: TokenType = lookahead(1)
        next match {
          case x if isLiteralToken(x) || x == RPAREN => tupleExpression()
          case _ => castExpression()
        }
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
      case _ => throw new JawaParserException(currentToken.pos, "Unexpected expression start: " + currentToken)
    }
  }
  
  private def nameExpression(): NameExpression = {
    if(currentTokenType != ID && currentTokenType != STATIC_ID)
      throw new JawaParserException(currentToken.pos, "expected 'ID' or 'STATIC_ID' but " + currentToken + " found")
    val nameID: Token = nextToken()
    val annotations_ : IList[Annotation] = annotations()
    val ne = NameExpression(nameID, annotations_)
    ne
  }
  
  private def indexingExpression(): IndexingExpression = {
    val baseID: Token = accept(ID)
    val indices: IList[IndexingSuffix] = indexingSuffixs()
    val ie = IndexingExpression(baseID, indices)
    ie
  }
  
  private def indexingSuffixs(): IList[IndexingSuffix] = {
    val indices: MList[IndexingSuffix] = mlistEmpty
    def loop(){
      currentTokenType match {
        case LBRACKET =>
          val lbracket: Token = nextToken()
          val exp: Expression = expression()
          val rbracket: Token = accept(RBRACKET)
          val is = IndexingSuffix(lbracket, exp, rbracket)
          indices += is
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
    val ae = AccessExpression(baseID, dot, fieldID)
    ae
  }
  
  private def tupleExpression(): TupleExpression = {
    val lparen: Token = accept(LPAREN)
    val constants: MList[(Token, Option[Token])] = mlistEmpty
    while(currentTokenType != RPAREN) {
      if(!isLiteral) throw new JawaParserException(currentToken.pos, "expected literal but found " + currentToken)
      val cons: Token = nextToken()
      val commaOpt: Option[Token] = 
        currentTokenType match {
          case COMMA => Some(nextToken())
          case _ => None
        }
      constants += ((cons, commaOpt))
    }
    val rparen: Token = accept(RPAREN)
    val te = TupleExpression(lparen, constants.toList, rparen)
    te
  }
  
  private def castExpression(): CastExpression = {
    val lparen: Token = accept(LPAREN)
    val typ_ : Type = typ(withinit = false)
    val rparen: Token = accept(RPAREN)
    val exp: Expression = expression()
    val ce = CastExpression(lparen, typ_, rparen, exp)
    ce
  }
  
  private def newExpression(): NewExpression = {
    val newToken: Token = accept(NEW)
    val typ_ : Type = typ(withinit = true)
    val ne = NewExpression(newToken, typ_)
    ne
  }
  
  private def literalExpression(): LiteralExpression = {
    if(!isLiteral) throw new JawaParserException(currentToken.pos, "expected literal but found " + currentToken)
    val constant: Token = nextToken()
    val le = LiteralExpression(constant)
    le
  }
  
  private def unaryExpression(): UnaryExpression = {
    val op: Token = accept(OP) // need to check is it unary op
    val exp: Expression = expression()
    val ue = UnaryExpression(op, exp)
    ue
  }
  
  private def binaryExpression(): BinaryExpression = {
    if(currentTokenType != ID && 
       currentTokenType != INTEGER_LITERAL &&
       currentTokenType != FLOATING_POINT_LITERAL)
      throw new JawaParserException(currentToken.pos, "expected 'ID' or 'INTEGER_LITERAL' or 'FLOATING_POINT_LITERAL' but " + currentToken + " found")
    val left: Token = nextToken()
    val op: Token = accept(OP) // need to check is it binary op
    if(currentTokenType != ID && 
       currentTokenType != INTEGER_LITERAL &&
       currentTokenType != FLOATING_POINT_LITERAL)
      throw new JawaParserException(currentToken.pos, "expected 'ID' or 'INTEGER_LITERAL' or 'FLOATING_POINT_LITERAL' but " + currentToken + " found")
    val right: Token = nextToken()
    val be = BinaryExpression(left, op, right)
    be
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
  
  private def cmpExpression(): CmpExpression = {
    val cmp: Token = accept(CMP)
    val lparen: Token = accept(LPAREN)
    val var1ID: Token = accept(ID)
    val comma: Token = accept(COMMA)
    val var2ID: Token = accept(ID)
    val rparen: Token = accept(RPAREN)
    val ce = CmpExpression(cmp, lparen, var1ID, comma, var2ID, rparen)
    ce
  }
  
  private def catchClause(): CatchClause = {
    val catchToken: Token = accept(CATCH)
    val typOrToken: Either[Type, Token] = currentTokenType match {
      case ANY => Right(accept(ANY))
      case _ => Left(typ(withinit = false))
    }
    val range: CatchRange = catchRange()
    val goto: Token = accept(GOTO)
    val targetLocation: Token = accept(ID)
    val semi: Token = accept(SEMI)
    val cc = CatchClause(catchToken, typOrToken, range, goto, targetLocation, semi)
    cc
  }
  
  private def catchRange(): CatchRange = {
    val at: Token = accept(AT)
    val lbracket: Token = accept(LBRACKET)
    val fromLocation: Token = accept(ID)
    val range: Token = accept(RANGE)
    val toLocation: Token = accept(ID)
    val rbracket: Token = accept(RBRACKET)
    val cr = CatchRange(at, lbracket, fromLocation, range, toLocation, rbracket)
    cr
  }
  
  private def typ(withinit: Boolean): Type = {
    val baseTypeID: Token = accept(ID)
    val typeFragments: MList[TypeFragment] = mlistEmpty
    def loop() {
      currentTokenType match {
        case LBRACKET =>
          if(withinit){
            typeFragments += typeFragmentWithInit()
          } else {
            typeFragments += rawTypeFragment()
          }
          loop()
        case _ =>
      }
    }
    loop()
    val typ = Type(baseTypeID, typeFragments.toList)
    typ
  }
  
  private def rawTypeFragment(): RawTypeFragment = {
    val lbracket: Token = nextToken()
    val rbracket: Token = accept(RBRACKET)
    val rtf = RawTypeFragment(lbracket, rbracket)
    rtf
  }
  
  private def typeFragmentWithInit(): TypeFragmentWithInit = {
    val lbracket: Token = nextToken()
    val varIDs: MList[(Token, Option[Token])] = mlistEmpty
    while(currentTokenType != RBRACKET) {
      val varID: Token = accept(ID)
      val commaOpt: Option[Token] = 
        currentTokenType match {
          case COMMA => Some(nextToken)
          case _ => None
        }
      varIDs += ((varID, commaOpt))
    }
    val rbracket: Token = accept(RBRACKET)
    val tfwi = TypeFragmentWithInit(lbracket, varIDs.toList, rbracket)
    tfwi
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
      throw new JawaParserException(currentToken.pos, "Expected token " + tokenType + " but got " + currentToken)

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
  
  import scala.reflect.runtime.{ universe => ru }
  final val COMPILATION_UNIT_TYPE = ru.typeOf[CompilationUnit]
  final val CLASS_OR_INTERFACE_DECLARATION_TYPE = ru.typeOf[ClassOrInterfaceDeclaration]
  final val METHOD_DECLARATION_TYPE = ru.typeOf[MethodDeclaration]
  final val BODY_TYPE = ru.typeOf[Body]
  final val LOCATION_TYPE = ru.typeOf[Location]
  
  /**
   * parse the given source as a parsable ast node
   */
  def parse[T <: ParsableAstNode : ru.TypeTag](source: Either[String, AbstractFile], resolveBody: Boolean, reporter: Reporter): Option[T] = {
    val tokens = JawaLexer.tokenise(source, reporter)
    parse(tokens, resolveBody, reporter)
  }
  
  def parse[T <: ParsableAstNode : ru.TypeTag](tokens: IList[Token], resolveBody: Boolean, reporter: Reporter): Option[T] = {
    val parser = new JawaParser(tokens.toArray, reporter)
    try{
      val pasable: T =
        (ru.typeOf[T] match {
            case t if t =:= COMPILATION_UNIT_TYPE =>
              parser.compilationUnit(resolveBody)
            case t if t =:= CLASS_OR_INTERFACE_DECLARATION_TYPE =>
              parser.classOrInterfaceDeclaration(resolveBody)
            case t if t =:= METHOD_DECLARATION_TYPE =>
              parser.methodDeclaration(resolveBody)
            case t if t =:= BODY_TYPE =>
              parser.body(resolveBody)
            case t if t=:= LOCATION_TYPE =>
              parser.location
        }).asInstanceOf[T]
      Some(pasable)
    } catch {
      case e: JawaParserException ⇒
        reporter.error(e.pos, e.message)
        None
    }
  }
}