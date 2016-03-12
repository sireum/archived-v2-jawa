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
package org.sireum.jawa.sjc.parser

import org.sireum.jawa.sjc.lexer._
import org.sireum.jawa.sjc.lexer.Tokens._
import org.sireum.util._
import org.sireum.jawa.Reporter
import org.sireum.jawa.JavaKnowledge
import org.sireum.jawa.io.SourceFile

class JawaParser(tokens: Array[Token], reporter: Reporter) extends JavaKnowledge {
  private val logging: Boolean = false

  import JawaParser._

  def safeParse[T <: ParsableAstNode](production: => T): Option[T] = try Some(production) catch { case e: JawaParserException => None }

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
        val typ = cid.cityp
        cid.getAllChildrenInclude foreach (_.enclosingTopLevelClass = typ)
    }
    cu
  }
  
  def classOrInterfaceDeclaration(resolveBody: Boolean): ClassOrInterfaceDeclaration = classOrInterfaceDeclaration0(resolveBody)
  
  private def classOrInterfaceDeclaration0(resolveBody: Boolean): ClassOrInterfaceDeclaration = {
    val dclToken: Token = accept(CLASS_OR_INTERFACE)
    val cityp: TypeDefSymbol = typeDefSymbol()
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
          val at = accept(AT)
          val annotationID: Token = accept(ID)
          val annotationValueOpt: Option[AnnotationValue] =
            annotationID.text match {
              case "type" | "owner" | "classDescriptor" => Some(TypeExpressionValue(typExpression()))
              case "signature" => Some(SymbolValue(signatureSymbol()))
              case _ =>
                currentTokenType match{
                  case ID => Some(TokenValue(nextToken()))
                  case x if isLiteralToken(x) => Some(TokenValue(nextToken()))
                  case _ => None
                }
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
  
  private def typeDefSymbol(): TypeDefSymbol = {
    val id = accept(ID)
    TypeDefSymbol(id)
  }
  
  private def typeSymbol(): TypeSymbol = {
    val id = accept(ID)
    TypeSymbol(id)
  }
  
  private def methodDefSymbol(): MethodDefSymbol = {
    val id = accept(ID)
    MethodDefSymbol(id)
  }
  
  private def methodNameSymbol(): MethodNameSymbol = {
    val id = accept(ID)
    MethodNameSymbol(id)
  }
  
  private def fieldDefSymbol(): FieldDefSymbol = {
    val id = accept(ID)
    FieldDefSymbol(id)
  }
  
  private def staticFieldDefSymbol(): FieldDefSymbol = {
    val id = accept(STATIC_ID)
    FieldDefSymbol(id)
  }
  
  private def fieldNameSymbol(): FieldNameSymbol = {
    val id = accept(ID)
    FieldNameSymbol(id)
  }
  
  private def staticFieldNameSymbol(): FieldNameSymbol = {
    val id = accept(STATIC_ID)
    FieldNameSymbol(id)
  }
  
  private def signatureSymbol(): SignatureSymbol = {
    val id = accept(ID)
    SignatureSymbol(id)
  }
  
  private def varDefSymbol(): VarDefSymbol = {
    val id = accept(ID)
    VarDefSymbol(id)
  }
  
  private def varSymbol(): VarSymbol = {
    val id = accept(ID)
    VarSymbol(id)
  }
  
  private def locationDefSymbol(): LocationDefSymbol = {
    val id = accept(LOCATION_ID)
    LocationDefSymbol(id)
  }
  
  private def locationSymbol(): LocationSymbol = {
    val id = accept(ID)
    LocationSymbol(id)
  }
  
  private def extendsAndImplimentsClausesOpt(): Option[ExtendsAndImplimentsClauses] = {
    currentTokenType match {
      case EXTENDS_AND_IMPLEMENTS =>
        val extendsAndImplementsToken: Token = accept(EXTENDS_AND_IMPLEMENTS)
        val parents: MList[(ExtendAndImpliment, Option[Token])] = mlistEmpty
        def loop() {
          val parent : ExtendAndImpliment = extendAndImpliment()
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
  
  private def extendAndImpliment(): ExtendAndImpliment = {
    val parenttyp: TypeSymbol = typeSymbol()
    val annotations_ : IList[Annotation] = annotations()
    ExtendAndImpliment(parenttyp, annotations_)
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
      val typ_ : Type = typ()
      val defSymbol: FieldDefSymbol = fieldDefSymbol()
      val annotations_ = annotations()
      val semi: Token = accept(SEMI)
      val insf = InstanceFieldDeclaration(typ_, defSymbol, annotations_, semi)
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
          val typ_ : Type = typ()
          val defSymbol: FieldDefSymbol = staticFieldDefSymbol()
          val annotations_ = annotations()
          val semi: Token = accept(SEMI)
          val sfd = StaticFieldDeclaration(staticFieldToken, typ_, defSymbol, annotations_, semi)
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
    val returnType: Type = typ()
    val defSymbol: MethodDefSymbol = methodDefSymbol()
    val paramClause_ : ParamClause = paramClause()
    val annotations_ : IList[Annotation] = annotations()
    val body_ : Body = body0(resolveBody)
    val md = MethodDeclaration(dclToken, returnType, defSymbol, paramClause_, annotations_, body_)
    defSymbol.signature = md.signature
    md.getAllChildren foreach {
      case ast =>
        ast match {
          case vd: VarDefSymbol => vd.owner = md
          case vs: VarSymbol => vs.owner = md
          case ld: LocationDefSymbol => ld.owner = md
          case ls: LocationSymbol => ls.owner = md
          case _ =>
        }
    }
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
    val typ_ : Type = typ()
    val defSymbol: VarDefSymbol = varDefSymbol()
    val annotations_ = annotations()
    val p = Param(typ_, defSymbol, annotations_)
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
      rb.getAllChildren foreach {
        c =>
          c match {
            case ls: LocationSym =>
              rb.locations.find(_.locationUri == ls.location) match{
                case Some(l) => 
                  ls.locationIndex = l.locationSymbol.locationIndex
                case None =>
              }
            case _ =>
          }
      }
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
    while(currentTokenType != LOCATION_ID && currentTokenType != RBRACE && currentTokenType != CATCH){
      val ahead1 = lookahead(1)
      val typOpt: Option[Type] = ahead1 match {
        case SEMI =>
          None
        case _ =>
          Some(typ())
      }
      val varSymbol_ : VarDefSymbol = varDefSymbol()
      val semi: Token = accept(SEMI)
      val local = LocalVarDeclaration(typOpt, varSymbol_, semi)
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
    val locationSymbol_ : LocationDefSymbol = locationDefSymbol()
    locationSymbol_.locationIndex = index
    val statement_ : Statement = statement()
    val semiOpt: Option[Token] = 
      currentTokenType match {
        case SEMI => Some(nextToken())
        case _ => None
      }
    val l = Location(locationSymbol_, statement_, semiOpt)
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
      case AT if lookahead(1) == MONITOR_ENTER || lookahead(1) == MONITOR_EXIT => monitorStatement()
      case AT | SEMI | LOCATION_ID | RBRACE | CATCH => emptyStatement()
      case _ => assignmentStatement()
    }
  }
  
  private def callStatement(): CallStatement = {
    val callToken: Token = accept(CALL)
    val ahead2 = lookahead(1)
    val lhsOpt: Option[CallLhs] = ahead2 match {
      case ASSIGN_OP =>
        Some(callLhs())
      case _ => None
    }
    val nameSymbol: MethodNameSymbol = methodNameSymbol()
    val argClause_ : ArgClause = argClause()
    val annotations_ : IList[Annotation] = annotations()
    val cs = CallStatement(callToken, lhsOpt, nameSymbol, argClause_, annotations_)
    nameSymbol.signature = cs.signature
    cs
  }
  
  private def callLhs(): CallLhs = {
    val lhs: VarSymbol = varSymbol()
    val assignmentOP: Token = accept(ASSIGN_OP)
    CallLhs(lhs, assignmentOP)
  }
  
  private def argClause(): ArgClause = {
    val lparen: Token = accept(LPAREN)
    val varIDs: MList[(VarSymbol, Option[Token])] = mlistEmpty
    while(currentTokenType != RPAREN) {
      val varSymbol_ : VarSymbol = varSymbol()
      val commaOpt: Option[Token] =
        currentTokenType match {
          case COMMA => Some(nextToken())
          case _ => None
        }
      varIDs += ((varSymbol_, commaOpt))
    }
    val rparen: Token = accept(RPAREN)
    val ac = ArgClause(lparen, varIDs.toList, rparen)
    ac
  }
  
  private def assignmentStatement(): AssignmentStatement = {
    val lhs: Expression with LHS = expression_lhs()
    val assignOP: Token = accept(ASSIGN_OP)
    val rhs: Expression with RHS = expression_rhs()
    val annotations_ : IList[Annotation] = annotations()
    val as = AssignmentStatement(lhs, assignOP, rhs, annotations_)
    as
  }
  
  private def throwStatement(): ThrowStatement = {
    val throwToken: Token = accept(THROW)
    val varSymbol_ : VarSymbol = varSymbol()
    val ts = ThrowStatement(throwToken, varSymbol_)
    ts
  }
  
  private def ifStatement(): IfStatement = {
    val ifToken: Token = accept(IF)
    val cond: BinaryExpression = binaryExpression()
    val thengoto: (Token, Token) = (accept(THEN), accept(GOTO))
    val targetLocation: LocationSymbol = locationSymbol()
    val is = IfStatement(ifToken, cond, thengoto, targetLocation)
    is
  }
  
  private def gotoStatement(): GotoStatement = {
    val goto: Token = accept(GOTO)
    val targetLocation: LocationSymbol = locationSymbol()
    val gs = GotoStatement(goto, targetLocation)
    gs
  }
  
  private def switchStatement(): SwitchStatement = {
    val switchToken: Token = accept(SWITCH)
    val condition: VarSymbol = varSymbol()
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
            val targetLocation: LocationSymbol = locationSymbol()
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
          val targetLocation: LocationSymbol = locationSymbol()
          val sd = SwitchDefaultCase(bar, elseToken, arrow, goto, targetLocation)
          Some(sd)
        } else None
      case _ => None
    }
  }
  
  private def returnStatement(): ReturnStatement = {
    val returnToken: Token = accept(RETURN)
    val varOpt: Option[VarSymbol] =
      currentTokenType match {
        case ID => Some(varSymbol())
        case _ => None
      }
    val annotations_ : IList[Annotation] = annotations()
    val rs = ReturnStatement(returnToken, varOpt, annotations_)
    rs
  }
  
  private def monitorStatement(): MonitorStatement = {
    val at: Token = accept(AT)
    val monitor: Token = currentTokenType match {
      case MONITOR_ENTER => accept(MONITOR_ENTER)
      case MONITOR_EXIT => accept(MONITOR_EXIT)
      case _ => throw new JawaParserException(currentToken.pos, "Unexpected expression start: " + currentToken)
    }
    val varSymbol_ : VarSymbol = varSymbol()
    MonitorStatement(at, monitor, varSymbol_)
  }
  
  private def emptyStatement(): EmptyStatement = {
    val annotations_ : IList[Annotation] = annotations()
    val es = EmptyStatement(annotations_)
    es
  }
  
  private def expression_lhs(): Expression with LHS = {
    currentTokenType match {
      case STATIC_ID =>
        nameExpression()
      case ID =>
        val next: TokenType = lookahead(1)
        next match {
          case DOT => accessExpression()
          case LBRACKET => indexingExpression()
          case _ => nameExpression()
        }
      case _ =>  throw new JawaParserException(currentToken.pos, "Unexpected expression start: " + currentToken)
    }
  }
  
  private def expression_rhs(): Expression with RHS = {
    currentTokenType match {
      case NEW => newExpression()
      case CMP => cmpExpression()
      case EXCEPTION => exceptionExpression()
      case CONST_CLASS => constClassExpression()
      case LENGTH => lengthExpression()
      case INSTANCEOF => instanceofExpression()
      case NULL => nullExpression()
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
        val further: TokenType = lookahead(2)
        val isLength: Boolean = further match {
          case LENGTH => true
          case _ => false
        }
        next match {
          case DOT => accessExpression()
          case LBRACKET => indexingExpression()
          case OP => binaryExpression()
          case _ => nameExpression()
        }
      case _ => throw new JawaParserException(currentToken.pos, "Unexpected expression start: " + currentToken)
    }
  }
  
  private def nullExpression(): NullExpression = {
    val nul: Token = accept(NULL)
    NullExpression(nul)
  }
  
  private def constClassExpression(): ConstClassExpression = {
    val const_class: Token = accept(CONST_CLASS)
    val at: Token = accept(AT)
    val typeToken: Token = nextToken()
    val typExp: TypeExpression = typExpression()
    ConstClassExpression(const_class, at, typeToken, typExp)
  }
  
  private def lengthExpression():LengthExpression = {
    val length: Token = accept(LENGTH)
    val at: Token = accept(AT)
    val variable: Token = nextToken()
    val varSymbol_ : VarSymbol = varSymbol()
    LengthExpression(length, at, variable, varSymbol_)
  }
  
  private def instanceofExpression(): InstanceofExpression = {
    val instanceof: Token = accept(INSTANCEOF)
    val at1: Token = accept(AT)
    val variable: Token = nextToken()
    val varSymbol_ : VarSymbol = varSymbol()
    val at2: Token = accept(AT)
    val typeToken: Token = nextToken()
    val typExp : TypeExpression = typExpression()
    InstanceofExpression(instanceof, at1, variable, varSymbol_, at2, typeToken, typExp)
  }
  
  private def exceptionExpression(): ExceptionExpression = {
    val exception: Token = accept(EXCEPTION)
    ExceptionExpression(exception)
  }
  
  private def nameExpression(): NameExpression = {
    val varSymbol_ = 
      currentTokenType match {
        case ID => Left(varSymbol())
        case STATIC_ID => Right(staticFieldNameSymbol())
        case _ => throw new JawaParserException(currentToken.pos, "expected 'ID' or 'STATIC_ID' but " + currentToken + " found")
      }
    val ne = NameExpression(varSymbol_)
    ne
  }
  
  private def indexingExpression(): IndexingExpression = {
    val baseSymbol: VarSymbol = varSymbol()
    val indices: IList[IndexingSuffix] = indexingSuffixs()
    val ie = IndexingExpression(baseSymbol, indices)
    ie
  }
  
  private def indexingSuffixs(): IList[IndexingSuffix] = {
    val indices: MList[IndexingSuffix] = mlistEmpty
    def loop(){
      currentTokenType match {
        case LBRACKET =>
          val lbracket: Token = nextToken()
          val index: Either[VarSymbol, Token] = getVarOrLit()
          val rbracket: Token = accept(RBRACKET)
          val is = IndexingSuffix(lbracket, index, rbracket)
          indices += is
          loop()
        case _ =>
      }
    }
    loop()
    indices.toList
  }
  
  private def accessExpression(): AccessExpression = {
    val baseSymbol: VarSymbol = varSymbol()
    val dot: Token = accept(DOT)
    val fieldSym: FieldNameSymbol = fieldNameSymbol()
    val ae = AccessExpression(baseSymbol, dot, fieldSym)
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
    val typ_ : Type = typ()
    val rparen: Token = accept(RPAREN)
    val varSym: VarSymbol = varSymbol()
    val ce = CastExpression(lparen, typ_, rparen, varSym)
    ce
  }
  
  private def newExpression(): NewExpression = {
    val newToken: Token = accept(NEW)
    val baseTypeSymbol: Either[TypeSymbol, Token] = {
      currentToken match {
        case x if isJavaPrimitive(x.text) => Right(accept(ID))
        case _ => Left(typeSymbol())
      }
    }
    val typeFragments: MList[TypeFragmentWithInit] = mlistEmpty
    def loop() {
      currentTokenType match {
        case LBRACKET =>
          typeFragments += typeFragmentWithInit()
          loop()
        case _ =>
      }
    }
    loop()
    val ne = NewExpression(newToken, baseTypeSymbol, typeFragments.toList)
    ne
  }
  
  private def literalExpression(): LiteralExpression = {
    if(!isLiteral) throw new JawaParserException(currentToken.pos, "expected literal but found " + currentToken)
    val constant: Token = nextToken()
    val le = LiteralExpression(constant)
    le
  }
  
  private def getVarOrLit(): Either[VarSymbol, Token] = {
    currentTokenType match {
      case x if isLiteralToken(x) => Right(nextToken())
      case _ => Left(varSymbol())
    }
  }
  
  private def unaryExpression(): UnaryExpression = {
    val op: Token = accept(OP) // need to check is it unary op
    val unary: VarSymbol = varSymbol
    val ue = UnaryExpression(op, unary)
    ue
  }
  
  private def binaryExpression(): BinaryExpression = {
    if(currentTokenType != ID && 
       currentTokenType != INTEGER_LITERAL &&
       currentTokenType != FLOATING_POINT_LITERAL)
      throw new JawaParserException(currentToken.pos, "expected 'ID' or 'INTEGER_LITERAL' or 'FLOATING_POINT_LITERAL' but " + currentToken + " found")
    val left: VarSymbol = varSymbol
    val op: Token = accept(OP) // need to check is it binary op
    if(currentTokenType != ID && 
       currentTokenType != INTEGER_LITERAL &&
       currentTokenType != FLOATING_POINT_LITERAL &&
       currentTokenType != NULL)
      throw new JawaParserException(currentToken.pos, "expected 'ID' or 'INTEGER_LITERAL' or 'FLOATING_POINT_LITERAL' or 'NULL' but " + currentToken + " found")
    val right: Either[VarSymbol, Token] = getVarOrLit()
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
    val var1Symbol: VarSymbol = varSymbol()
    val comma: Token = accept(COMMA)
    val var2Symbol: VarSymbol = varSymbol()
    val rparen: Token = accept(RPAREN)
    val ce = CmpExpression(cmp, lparen, var1Symbol, comma, var2Symbol, rparen)
    ce
  }
  
  private def catchClause(): CatchClause = {
    val catchToken: Token = accept(CATCH)
    val typ_ : Type = typ()
    val range: CatchRange = catchRange()
    val goto: Token = accept(GOTO)
    val targetLocation: LocationSymbol = locationSymbol()
    val semi: Token = accept(SEMI)
    val cc = CatchClause(catchToken, typ_, range, goto, targetLocation, semi)
    cc
  }
  
  private def catchRange(): CatchRange = {
    val at: Token = accept(AT)
    val lbracket: Token = accept(LBRACKET)
    val fromLocation: LocationSymbol = locationSymbol()
    val range: Token = accept(RANGE)
    val toLocation: LocationSymbol = locationSymbol()
    val rbracket: Token = accept(RBRACKET)
    val cr = CatchRange(at, lbracket, fromLocation, range, toLocation, rbracket)
    cr
  }
  
  private def typExpression(): TypeExpression = {
    val hat: Token = accept(HAT)
    val typ_ : Type = typ()
    TypeExpression(hat, typ_)
  }
  
  private def typ(): Type = {
    val baseTypeSymbol: Either[TypeSymbol, Token] = {
      currentToken match {
        case x if isJavaPrimitive(x.text) => Right(accept(ID))
        case _ => Left(typeSymbol())
      }
    }
    val typeFragments: MList[TypeFragment] = mlistEmpty
    def loop() {
      currentTokenType match {
        case LBRACKET =>
          typeFragments += typeFragment()
          loop()
        case _ =>
      }
    }
    loop()
    val typ = Type(baseTypeSymbol, typeFragments.toList)
    typ
  }
  
  private def typeFragment(): TypeFragment = {
    val lbracket: Token = nextToken()
    val rbracket: Token = accept(RBRACKET)
    val rtf = TypeFragment(lbracket, rbracket)
    rtf
  }
  
  private def typeFragmentWithInit(): TypeFragmentWithInit = {
    val lbracket: Token = nextToken()
    val varSymbols: MList[(VarSymbol, Option[Token])] = mlistEmpty
    while(currentTokenType != RBRACKET) {
      val varSymbol_ : VarSymbol = varSymbol()
      val commaOpt: Option[Token] = 
        currentTokenType match {
          case COMMA => Some(nextToken)
          case _ => None
        }
      varSymbols += ((varSymbol_, commaOpt))
    }
    val rbracket: Token = accept(RBRACKET)
    val tfwi = TypeFragmentWithInit(lbracket, varSymbols.toList, rbracket)
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
    else {
      throw new JawaParserException(currentToken.pos, "Expected token " + tokenType + " but got " + currentToken)
    }

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

  private def log[T](s: String)(f: => T): T = {
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
  def parse[T <: ParsableAstNode : ru.TypeTag](source: Either[String, SourceFile], resolveBody: Boolean, reporter: Reporter): Option[T] = {
      val tokens = JawaLexer.tokenise(source, reporter)
      val res = parse(tokens, resolveBody, reporter)
      if(!res.isDefined) println(source)
      res
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
      case e: JawaParserException =>
        reporter.error(e.pos, e.message)
//        e.printStackTrace()
        None
    }
  }
}
