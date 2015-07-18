package org.sireum.jawa.sjc.codegen

import org.sireum.jawa.AccessFlag
import org.sireum.util._
import org.sireum.jawa.sjc.JavaKnowledge
import org.sireum.jawa.sjc.parser.{CompilationUnit => JawaCompilationUnit}
import org.sireum.jawa.sjc.parser.{ClassOrInterfaceDeclaration => ClassOrInterfaceDeclaration}
import org.sireum.jawa.sjc.parser.{Declaration => JawaDeclaration}
import org.sireum.jawa.sjc.parser.{Field => JawaField}
import org.sireum.jawa.sjc.parser.{Location => JawaLocation}
import org.sireum.jawa.sjc.parser.MethodDeclaration
import org.sireum.jawa.sjc.parser.ResolvedBody
import org.sireum.jawa.sjc.parser.UnresolvedBody
import org.sireum.jawa.sjc.JawaType
import org.objectweb.asm.Label
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes
import org.objectweb.asm.MethodVisitor
import org.sireum.jawa.sjc.parser.CallStatement
import org.sireum.jawa.sjc.parser.JawaSymbol
import org.sireum.jawa.sjc.parser.VarSymbol
import org.sireum.jawa.sjc.ObjectType
import org.sireum.jawa.sjc.PrimitiveType
import org.sireum.jawa.sjc.parser.AssignmentStatement
import org.sireum.jawa.sjc.parser.Expression
import org.sireum.jawa.sjc.parser.NameExpression
import org.sireum.jawa.sjc.parser.IndexingExpression
import org.sireum.jawa.sjc.parser.AccessExpression
import org.sireum.jawa.sjc.parser.TupleExpression
import org.sireum.jawa.sjc.parser.CastExpression
import org.sireum.jawa.sjc.parser.NewExpression
import org.sireum.jawa.sjc.parser.LiteralExpression
import org.sireum.jawa.sjc.parser.UnaryExpression
import org.sireum.jawa.sjc.parser.BinaryExpression
import org.sireum.jawa.sjc.parser.CmpExpression
import org.sireum.jawa.sjc.parser.RHS
import org.sireum.jawa.sjc.parser.LHS
import org.sireum.jawa.sjc.parser.ThrowStatement
import org.sireum.jawa.sjc.parser.IfStatement
import org.sireum.jawa.sjc.parser.GotoStatement
import org.sireum.jawa.sjc.parser.SwitchStatement
import org.sireum.jawa.sjc.parser.ReturnStatement
import org.sireum.jawa.sjc.parser.EmptyStatement
import org.sireum.jawa.sjc.parser.TypeFragmentWithInit
import java.io.PrintWriter
import scala.tools.asm.ClassReader
import scala.tools.asm.util.TraceClassVisitor
import org.sireum.jawa.sjc.parser.ExceptionExpression
import org.sireum.jawa.sjc.parser.MonitorStatement
import org.objectweb.asm.Type
import org.sireum.jawa.sjc.parser.InstanceofExpression
import org.sireum.jawa.sjc.parser.InstanceofExpression
import org.sireum.jawa.sjc.parser.ConstClassExpression
import org.sireum.jawa.sjc.parser.LengthExpression
import java.io.File
import java.io.FileWriter
import java.io.DataOutputStream
import java.io.FileOutputStream
import org.sireum.jawa.sjc.parser.NullExpression
import org.sireum.jawa.sjc.parser.UnaryExpression

object JavaByteCodeGenerator {
  def outputByteCodes(pw: PrintWriter, bytecodes: Array[Byte]) = {
    val cr = new ClassReader(bytecodes)
    val tcv = new TraceClassVisitor(pw)
    cr.accept(tcv, ClassReader.SKIP_FRAMES)
    pw.flush()
  }
  
  def writeClassFile(outputPath: String, pkg: String, className: String, bytecode: Array[Byte]): Unit = {
    val classfileDirPath: String = outputPath + File.separator + pkg.replaceAll("\\.", File.separator)
    val classfileDir: File = new File(classfileDirPath)
    if(!classfileDir.exists()){
      classfileDir.mkdirs()
    }
    val dout=new DataOutputStream(new FileOutputStream(new File(classfileDir, className + ".class")))
    dout.write(bytecode)
    dout.flush()
    dout.close()
  }
}

class JavaByteCodeGenerator {
  private val classes: MMap[ObjectType, Array[Byte]] = mmapEmpty
  
  def getClasses: IMap[ObjectType, Array[Byte]] = classes.toMap
  
  def generate(cu: JawaCompilationUnit): IMap[ObjectType, Array[Byte]] = {
    cu.topDecls foreach {
      cid =>
        visitClass(cid, Opcodes.V1_6)
    }
    getClasses
  }
  
  private def getClassName(name: String): String = {
    name.replaceAll("\\.", "/")
  }
  
  private def getJavaFlags(af: Int): Int = {
    var mod: Int = 0
    if(AccessFlag.isPrivate(af))
      mod = mod | Opcodes.ACC_PRIVATE
    else if (AccessFlag.isProtected(af))
      mod = mod | Opcodes.ACC_PROTECTED
    else if (AccessFlag.isPublic(af))
      mod = mod | Opcodes.ACC_PUBLIC
      
    if(AccessFlag.isAbstract(af))
      mod = mod | Opcodes.ACC_ABSTRACT
    if(AccessFlag.isAnnotation(af))
      mod = mod | Opcodes.ACC_ANNOTATION
//    if(AccessFlag.isConstructor(af))
//      mod = mod | Opcodes.ACC_
    if(AccessFlag.isDeclaredSynchronized(af))
      mod = mod | Opcodes.ACC_SYNCHRONIZED
    if(AccessFlag.isEnum(af))
      mod = mod | Opcodes.ACC_ENUM
    if(AccessFlag.isFinal(af))
      mod = mod | Opcodes.ACC_FINAL
    if(AccessFlag.isInterface(af))
      mod = mod | Opcodes.ACC_INTERFACE
    if(AccessFlag.isNative(af))
      mod = mod | Opcodes.ACC_NATIVE
    if(AccessFlag.isStatic(af))
      mod = mod | Opcodes.ACC_STATIC
    if(AccessFlag.isStrictFP(af))
      mod = mod | Opcodes.ACC_STRICT
    if(AccessFlag.isSynchronized(af))
      mod = mod | Opcodes.ACC_SYNCHRONIZED
    if(AccessFlag.isSynthetic(af))
      mod = mod | Opcodes.ACC_SYNTHETIC
    if(AccessFlag.isTransient(af))
      mod = mod | Opcodes.ACC_TRANSIENT
    if(AccessFlag.isVolatile(af))
      mod = mod | Opcodes.ACC_VOLATILE
    mod
  }
  
  private def visitClass(cid: ClassOrInterfaceDeclaration, javaVersion: Int): Unit = {
    val cw: ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
    val af: Int = AccessFlag.getAccessFlags(cid.accessModifier)
    var mod = getJavaFlags(af)
    
    val superName: String = cid.superClassOpt match {
      case Some(su) => getClassName(su.name)
      case None => if(cid.typ.name != JavaKnowledge.JAVA_TOPLEVEL_OBJECT) getClassName(JavaKnowledge.JAVA_TOPLEVEL_OBJECT) else null
    }
    val interfaceNames: IList[String] = cid.interfaces map(i => getClassName(i.name))
    if(superName != null || !interfaceNames.isEmpty) mod = mod | Opcodes.ACC_SUPER
    cw.visit(javaVersion, mod, getClassName(cid.typ.name), null, superName, interfaceNames.toArray)
    cw.visitSource(null, null)
    cid.fields foreach {
      fd => visitField(cw, fd)
    }
    cid.methods foreach {
      md => visitMethod(cw, md)
    }
    cw.visitEnd()
    
    this.classes(cid.typ) = cw.toByteArray()
  }
  
  private def visitField(cw: ClassWriter, fd: JawaField with JawaDeclaration): Unit = {
    val af: Int = AccessFlag.getAccessFlags(fd.accessModifier)
    val mod: Int = getJavaFlags(af)
    val typ: String = JavaKnowledge.formatTypeToSignature(fd.typ.typ)
    cw.visitField(mod, fd.fieldName, typ, null, null).visitEnd()
  }
  
  case class LocalIndex(varname: String, typ: JawaType, index: Int)
  private val locals: MMap[String, LocalIndex] = mmapEmpty
  private val locations: MMap[String, Label] = mmapEmpty
  private var maxLocals: Int = 0
  
  private def visitMethod(cw: ClassWriter, md: MethodDeclaration): Unit = {
    val af: Int = AccessFlag.getAccessFlags(md.accessModifier)
    val mod: Int = getJavaFlags(af)
    val mv = cw.visitMethod(mod, md.name, md.signature.getDescriptor, null, null)
    
    val body: ResolvedBody = md.body match {
      case rb: ResolvedBody =>
        rb
      case ub: UnresolvedBody =>
        ub.resolve
    }
    var i = 0
    md.thisParam.foreach{
      t =>
        locals(t.name) = LocalIndex(t.name, t.typ.typ, i)
        i += 1
    }
    md.paramlist.foreach{
      param =>
        locals(param.name) = LocalIndex(param.name, param.typ.typ, i)
        if(param.typ.typ.name == "long" || param.typ.typ.name == "double") i += 2
        else i += 1
    }
    body.locals.foreach{
      local =>
        locals(local.varSymbol.varName) = LocalIndex(local.varSymbol.varName, local.typ, i)
        if(local.typ.name == "long" || local.typ.name == "double") i += 2
        else i += 1
    }
    this.maxLocals = this.locals.size
    body.locations foreach {
      location =>
        val locLabel = new Label()
        this.locations(location.locationUri) = locLabel
    }
    body.catchClauses foreach {
      catchClause =>
        val from: Label = this.locations(catchClause.range.fromLocation.location)
        val to: Label = this.locations(catchClause.range.toLocation.location)
        val target: Label = this.locations(catchClause.targetLocation.location)
        val typ: String = getClassName(catchClause.typ.typ.name)
        mv.visitTryCatchBlock(from, to, target, typ)
    }
    val initLabel = new Label()
    mv.visitCode()
    mv.visitLabel(initLabel)
    
    body.locations foreach {
      location =>
        val locLabel = this.locations(location.locationUri)
        mv.visitLabel(locLabel)
//        println(location.locationUri) // to show the offset
//        println(locLabel.getOffset)
        mv.visitLineNumber(location.firstToken.pos.line, locLabel)
        visitLocation(mv, location)
    }
    
    val endLabel = new Label()
    mv.visitLabel(endLabel)
    this.locals foreach {
      case (name, local) =>
        mv.visitLocalVariable(local.varname, JavaKnowledge.formatTypeToSignature(local.typ), null, initLabel, endLabel, local.index)
    }
    try {
      mv.visitMaxs(0, 0)
    } catch {
      case ie: Exception =>
        throw new IndexOutOfBoundsException(md.signature + ":" + ie.getMessage)
    }
    mv.visitEnd()
    this.locals.clear()
    this.locations.clear()
    this.maxLocals = 0
  }
  
  private def visitLocation(mv: MethodVisitor, jl: JawaLocation): Unit = {
    jl.statement match {
      case cs: CallStatement =>
        visitCallStatement(mv, cs)
      case as: AssignmentStatement =>
        visitAssignmentStatement(mv, as)
      case ts: ThrowStatement =>
        visitThrowStatement(mv, ts)
      case is: IfStatement =>
        visitIfStatement(mv, is)
      case gs: GotoStatement =>
        visitGotoStatement(mv, gs)
      case ss: SwitchStatement =>
        visitSwitchStatement(mv, ss)
      case rs: ReturnStatement =>
        visitReturnStatement(mv, rs)
      case ms: MonitorStatement =>
        visitMonitorStatement(mv, ms)
      case es: EmptyStatement =>
        
      case _ =>
    }
  }
  
  private def visitMonitorStatement(mv: MethodVisitor, ms: MonitorStatement): Unit = {
    import org.sireum.jawa.sjc.lexer.Tokens._  
    ms.monitor.tokenType match {
      case MONITOR_ENTER => 
        visitVarLoad(mv, ms.varSymbol.varName)
        mv.visitInsn(Opcodes.DUP)
        this.maxLocals += 1
        mv.visitVarInsn(Opcodes.ASTORE, this.maxLocals)
        mv.visitInsn(Opcodes.MONITORENTER)
      case MONITOR_EXIT => 
        mv.visitVarInsn(Opcodes.ALOAD, this.maxLocals)
        this.maxLocals -= 1
        mv.visitInsn(Opcodes.MONITOREXIT)
      case _ => println("visitMonitorStatement problem: " + ms)
    }
  }
  
  private def visitThrowStatement(mv: MethodVisitor, ts: ThrowStatement): Unit = {
    visitVarLoad(mv, ts.varSymbol.varName)
    mv.visitInsn(Opcodes.ATHROW)
  }
  
  private def visitSwitchStatement(mv: MethodVisitor, ss: SwitchStatement): Unit = {
    val dflt: Label = ss.defaultCaseOpt match {
      case Some(dc) => locations(dc.targetLocation.location)
      case None => locations(ss.cases.last.targetLocation.location)
    }
    val key = ss.condition.varName
    visitVarLoad(mv, key)
    val size = ss.cases.size
    val keys: MList[Int] = mlistEmpty
    val labels: MList[Label] = mlistEmpty
    
    for(i <- 0 to size - 1){
      val ca = ss.cases(i)
      keys += ca.constant.text.toInt
      labels += locations(ca.targetLocation.location)
    }
    mv.visitLookupSwitchInsn(dflt, keys.toArray, labels.toArray)
  }
  
  private def visitGotoStatement(mv: MethodVisitor, gs: GotoStatement): Unit = {
    val target = this.locations(gs.targetLocation.location)
    mv.visitJumpInsn(Opcodes.GOTO, target)
  }
  
  private def visitIfStatement(mv: MethodVisitor, is: IfStatement): Unit = {
    var isNull: Boolean = false
    var isObject: Boolean = false
    var isBoolean: Boolean = false
    val left = is.cond.left.varName
    locals(left).typ match {
      case PrimitiveType("boolean") =>
        isBoolean = true
      case _ =>
    }
    is.cond.right match {
      case Left(right) =>
        if(locals(right.varName).typ.isInstanceOf[ObjectType]) isObject = true
      case Right(right) => 
        right.text match {
          case "null" => isNull = true
          case t =>
        }
    }
    visitVarLoad(mv, left)
    
    is.cond.right match {
      case Left(right) => 
        visitVarLoad(mv, right.varName)
      case Right(right) => 
        right.text match {
          case "null" =>
          case t => if(!isBoolean)generateIntConst(mv, t.toInt)
        }
    }
    val target = this.locations(is.targetLocation.location)
    if(isNull){
      is.cond.op.text match {
        case "==" => mv.visitJumpInsn(Opcodes.IFNULL, target)
        case "!=" => mv.visitJumpInsn(Opcodes.IFNONNULL, target)
      }
    } else if(isObject) {
      is.cond.op.text match {
        case "==" => mv.visitJumpInsn(Opcodes.IF_ACMPEQ, target)
        case "!=" => mv.visitJumpInsn(Opcodes.IF_ACMPNE, target)
      }
    } else if (isBoolean) {
      is.cond.op.text match {
        case "==" => mv.visitJumpInsn(Opcodes.IFEQ, target)
        case "!=" => mv.visitJumpInsn(Opcodes.IFNE, target)
      }
    } else {
      is.cond.op.text match {
        case "==" => mv.visitJumpInsn(Opcodes.IF_ICMPEQ, target)
        case "!=" => mv.visitJumpInsn(Opcodes.IF_ICMPNE, target)
        case "<" =>  mv.visitJumpInsn(Opcodes.IF_ICMPLT, target)
        case ">=" => mv.visitJumpInsn(Opcodes.IF_ICMPGE, target)
        case ">" =>  mv.visitJumpInsn(Opcodes.IF_ICMPGT, target)
        case "<=" => mv.visitJumpInsn(Opcodes.IF_ICMPLE, target)
        case _ =>    println("visitIfStatement problem: " + is)
      }
    }
  }
  
  private def visitReturnStatement(mv: MethodVisitor, rs: ReturnStatement): Unit = {
    rs.varOpt match {
      case Some(va) => 
        visitVarLoad(mv, va.varName)
        this.locals(va.varName).typ.name match {
          case "int" => mv.visitInsn(Opcodes.IRETURN)
          case "long" => mv.visitInsn(Opcodes.LRETURN)
          case "float" => mv.visitInsn(Opcodes.FRETURN)
          case "double" => mv.visitInsn(Opcodes.DRETURN)
          case _ => mv.visitInsn(Opcodes.ARETURN)
//          case "double" =>
//          case "float" =>
//          case "int" =>
//          case "long" =>
//          case _ => println("visitReturnStatement problem: " + rs + " " + kind)
        }
      case None => 
        mv.visitInsn(Opcodes.RETURN)
    }
  }
  
  private def visitCallStatement(mv: MethodVisitor, cs: CallStatement): Unit = {
    val isReturnObject: Boolean = cs.signature.isReturnNonNomal()
    val isReturnVoid: Boolean = cs.signature.getReturnType().name == "void"
    val isStatic: Boolean = cs.isStatic
    cs.recvOpt match {
      case Some(recv) =>
        require(!isStatic, cs.toString())
        mv.visitVarInsn(Opcodes.ALOAD, this.locals(recv).index)
      case None =>
        require(isStatic)
    }
    for(i <- 0 to cs.signature.getParameterNum() - 1){
      val arg = cs.arg(i)
      val reqtyp: Option[PrimitiveType] = cs.signature.getParameterTypes()(i) match {
        case p: PrimitiveType => Some(p)
        case t: JawaType => None
      }
      val acttyp: Option[PrimitiveType] = this.locals(arg).typ match {
        case p: PrimitiveType => Some(p)
        case t: JawaType => None
      }
      visitVarLoad(mv, arg)
      handleTypeImplicitConvert(mv, reqtyp, acttyp)
    }
        
    val opcode = 
      if(cs.isVirtual) Opcodes.INVOKEVIRTUAL
      else if(cs.isStatic) Opcodes.INVOKESTATIC
      else if(cs.isDirect || cs.isSuper) Opcodes.INVOKESPECIAL
      else if(cs.isInterface) Opcodes.INVOKEINTERFACE
      else Opcodes.INVOKEVIRTUAL

    val className: String = getClassName(cs.signature.getClassType.name)
    val methodName: String = cs.signature.methodNamePart
    val descriptor: String = cs.signature.getDescriptor
    val ltf = opcode == Opcodes.INVOKEINTERFACE
    mv.visitMethodInsn(opcode, className, methodName, descriptor, ltf)
    
    val ret = cs.signature.getReturnType()
    ret.name match {
      case "void" =>
      case typ => 
        val kind = typ match{
          case x if JavaKnowledge.isJavaPrimitive(x) => x
          case _ => "object"
        }
        cs.lhsOpt match {
          case Some(lhs) => visitVarStore(mv, lhs.lhs.varName)
          case _ => 
            if(typ == "long" || typ == "double") mv.visitInsn(Opcodes.POP2)
            else mv.visitInsn(Opcodes.POP)
        }
    }
  }
  
  /**
   * typ could be:
   * move: "", long, object
   * return: "", long, object, Void
   * aget: int, long, object, boolean, byte, char, short
   * aput: int, long, object, boolean, byte, char, short
   * iget: int, long, boolean, byte, char, short, types
   * iput: int, long, boolean, byte, char, short, types
   * sget: int, long, boolean, byte, char, short, types
   * sput: int, long, boolean, byte, char, short, types
   * unop: int, long, float, double
   * biop: int, long, float, double
   * const: int, long, float, double
   * lit: int
   * cast: i2l, i2f, i2d, l2i, l2f, l2d, f2i, f2l, f2d, d2i, d2l, d2f, i2b, i2c, i2s, object
   */
  private def visitAssignmentStatement(mv: MethodVisitor, as: AssignmentStatement): Unit = {
    val kind: String = as.kind
    val typOpt: Option[JawaType] = as.typOpt
    val lhs = as.lhs
    val rhs = as.rhs
    
    //This is used to deal with implicit type conversion
    var lhsTyp: Option[PrimitiveType] = None
    
    lhs match {
      case ie: IndexingExpression =>
        visitArrayAccess(mv, ie)
        val tmp = JawaType.generateType(this.locals(ie.base).typ.typ, this.locals(ie.base).typ.dimensions - ie.dimentions)
        tmp match{
          case p: PrimitiveType => lhsTyp = Some(p)
          case _ =>
        }
      case ae: AccessExpression =>
        visitFieldAccess(mv, ae)
        typOpt.get match {
          case p: PrimitiveType => lhsTyp = Some(p)
          case _ =>
        }
      case ne: NameExpression =>
        ne.varSymbol match {
          case Left(v) =>
            this.locals(v.varName).typ match{
              case p: PrimitiveType => lhsTyp = Some(p)
              case _ =>
            }
          case Right(f) =>
            typOpt.get match {
              case p: PrimitiveType => lhsTyp = Some(p)
              case _ =>
            }
        }
      case _ =>
    }
    
    rhs match {
      case te: TupleExpression =>
        if(lhs.isInstanceOf[NameExpression]){
          lhs.asInstanceOf[NameExpression].varSymbol match {
            case Left(varSym) =>
              visitVarLoad(mv, varSym.varName)
            case Right(fnSym) =>
              val t = JavaKnowledge.getTypeFromName(kind)
              mv.visitFieldInsn(Opcodes.GETSTATIC, fnSym.baseType.name.replaceAll("\\.", "/"), fnSym.fieldName, JavaKnowledge.formatTypeToSignature(t))
          }
        }
      case _ =>
    }
    
    visitRhsExpression(mv, rhs, kind, typOpt, lhsTyp)
    visitLhsExpression(mv, lhs, kind, typOpt)
  }
  
  private def visitLhsExpression(mv: MethodVisitor, lhs: Expression with LHS, kind: String, typOpt: Option[JawaType]): Unit = lhs match {
    case ne: NameExpression =>
      ne.varSymbol match {
        case Left(varSym) =>
          visitVarStore(mv, ne.name)
        case Right(fnSym) =>
          val t = JavaKnowledge.getTypeFromName(kind)
          mv.visitFieldInsn(Opcodes.PUTSTATIC, fnSym.baseType.name.replaceAll("\\.", "/"), fnSym.fieldName, JavaKnowledge.formatTypeToSignature(typOpt.get))
      }
    case ie: IndexingExpression =>
      visitIndexStore(mv, ie, kind)
    case ae: AccessExpression =>
      visitFieldStore(mv, ae, typOpt.get)
    case _ => println("visitLhsExpression problem: " + lhs + " " + kind)
  }
  
  private def visitRhsExpression(mv: MethodVisitor, rhs: Expression with RHS, kind: String, typOpt: Option[JawaType], lhsTyp: Option[PrimitiveType]): Unit = rhs match {
    case ne: NameExpression =>
      ne.varSymbol match {
        case Left(varSym) =>
          visitVarLoad(mv, ne.name)
          var rhsTyp: Option[PrimitiveType] = None
          this.locals(varSym.varName).typ match {
            case pt: PrimitiveType =>
              rhsTyp = Some(pt)
            case _ =>
          }
          handleTypeImplicitConvert(mv, lhsTyp, rhsTyp)
        case Right(fnSym) =>
          mv.visitFieldInsn(Opcodes.GETSTATIC, fnSym.baseType.name.replaceAll("\\.", "/"), fnSym.fieldName, JavaKnowledge.formatTypeToSignature(typOpt.get))
          var rhsTyp: Option[PrimitiveType] = None
          typOpt.get match {
            case pt: PrimitiveType =>
              rhsTyp = Some(pt)
            case _ =>
          }
          handleTypeImplicitConvert(mv, lhsTyp, rhsTyp)
      }
    case ee: ExceptionExpression =>
    case ne: NullExpression =>
      mv.visitInsn(Opcodes.ACONST_NULL)
    case ie: IndexingExpression =>
      visitIndexLoad(mv, ie, kind)
      var rhsTyp: Option[PrimitiveType] = None
      val tmp = JawaType.generateType(this.locals(ie.base).typ.typ, this.locals(ie.base).typ.dimensions - ie.dimentions)
      tmp match{
        case p: PrimitiveType => rhsTyp = Some(p)
        case _ =>
      }
      handleTypeImplicitConvert(mv, lhsTyp, rhsTyp)
    case ae: AccessExpression =>
      visitFieldLoad(mv, ae, typOpt.get)
      var rhsTyp: Option[PrimitiveType] = None
      typOpt.get match {
        case pt: PrimitiveType =>
          rhsTyp = Some(pt)
        case _ =>
      }
      handleTypeImplicitConvert(mv, lhsTyp, rhsTyp)
    case te: TupleExpression =>
      visitTupleExpression(mv, te)
    case ce: CastExpression =>
      visitCastExpression(mv, ce, kind)
    case ne: NewExpression =>
      visitNewExpression(mv, ne)
    case le: LiteralExpression =>
      visitLiteralExpression(mv, le, kind)
    case ue: UnaryExpression =>
      visitUnaryExpression(mv, ue)
      var rhsTyp: Option[PrimitiveType] = None
      this.locals(ue.unary.varName).typ match {
        case pt: PrimitiveType =>
          rhsTyp = Some(pt)
        case _ =>
      }
      handleTypeImplicitConvert(mv, lhsTyp, rhsTyp)
    case be: BinaryExpression =>
      visitBinaryExpression(mv, be, kind, lhsTyp)
    case ce: CmpExpression =>
      visitCmpExpression(mv, ce)
    case ie: InstanceofExpression =>
      visitInstanceofExpression(mv, ie)
    case ce: ConstClassExpression =>
      visitConstClassExpression(mv, ce, typOpt.get)
    case le: LengthExpression =>
      visitLengthExpression(mv, le)
    case _ =>  println("visitRhsExpression problem: " + rhs + " " + kind)
  }
  
  private def handleTypeImplicitConvert(mv: MethodVisitor, lhsTyp: Option[PrimitiveType], rhsTyp: Option[PrimitiveType]) = {
    if(lhsTyp.isDefined && rhsTyp.isDefined){
      val lhs = lhsTyp.get.name
      val rhs = rhsTyp.get.name
      (rhs, lhs) match {
        case ("int", "long") => 
          mv.visitInsn(Opcodes.I2L)
        case ("int", "float") => 
          mv.visitInsn(Opcodes.I2F)
        case ("int", "double") =>
          mv.visitInsn(Opcodes.I2D)
        case ("long", "int") =>
          mv.visitInsn(Opcodes.L2I)
        case ("long", "float") =>
          mv.visitInsn(Opcodes.L2F)
        case ("long", "double") =>
          mv.visitInsn(Opcodes.L2D)
        case ("float", "int") =>
          mv.visitInsn(Opcodes.F2I)
        case ("float", "long") =>
          mv.visitInsn(Opcodes.F2L)
        case ("float", "double") =>
          mv.visitInsn(Opcodes.F2D)
        case ("double", "int") =>
          mv.visitInsn(Opcodes.D2I)
        case ("double", "long") =>
          mv.visitInsn(Opcodes.D2L)
        case ("double", "float") =>
          mv.visitInsn(Opcodes.D2F)
        case ("int", "byte") =>
          mv.visitInsn(Opcodes.I2B)
        case ("int", "char") =>
          mv.visitInsn(Opcodes.I2C)
        case ("int", "short") =>
          mv.visitInsn(Opcodes.I2S)
        case _ =>
      }
    }
  }
  
  private def visitConstClassExpression(mv: MethodVisitor, ce: ConstClassExpression, typ: JawaType): Unit = {
    val c = Type.getType(JavaKnowledge.formatTypeToSignature(typ))
    mv.visitLdcInsn(c)
  }
  
  private def visitLengthExpression(mv: MethodVisitor, le: LengthExpression): Unit = {
    visitVarLoad(mv, le.varSymbol.varName)
    mv.visitInsn(Opcodes.ARRAYLENGTH)
  }
  
  private def visitInstanceofExpression(mv: MethodVisitor, ie: InstanceofExpression): Unit = {
    visitVarLoad(mv, ie.varSymbol.varName)
    val typ: JawaType = ie.typ.typ
    mv.visitTypeInsn(Opcodes.INSTANCEOF, getClassName(typ.name))
  }
  
  private def visitCmpExpression(mv: MethodVisitor, ce: CmpExpression): Unit = {
    val reqTyp: Option[PrimitiveType] = ce.cmp.text match {
      case "fcmpl" => 
        Some(PrimitiveType("float"))
      case "dcmpl" =>
        Some(PrimitiveType("double"))
      case "fcmpg" => 
        Some(PrimitiveType("float"))
      case "dcmpg" =>
        Some(PrimitiveType("double"))
      case "lcmp" =>
        Some(PrimitiveType("long"))
      case _ => None
    }
    val first = ce.var1Symbol.varName
    visitVarLoad(mv, first)
    var rhs1Typ: Option[PrimitiveType] = None
    this.locals(first).typ match {
      case pt: PrimitiveType =>
        rhs1Typ = Some(pt)
      case _ =>
    }
    handleTypeImplicitConvert(mv, reqTyp, rhs1Typ)
    val second = ce.var2Symbol.varName
    visitVarLoad(mv, second)
    var rhs2Typ: Option[PrimitiveType] = None
    this.locals(second).typ match {
      case pt: PrimitiveType =>
        rhs2Typ = Some(pt)
      case _ =>
    }
    handleTypeImplicitConvert(mv, reqTyp, rhs2Typ)
    ce.cmp.text match {
      case "fcmpl" => 
        mv.visitInsn(Opcodes.FCMPL)
      case "dcmpl" =>
        mv.visitInsn(Opcodes.DCMPL)
      case "fcmpg" => 
        mv.visitInsn(Opcodes.FCMPG)
      case "dcmpg" =>
        mv.visitInsn(Opcodes.DCMPG)
      case "lcmp" =>
        mv.visitInsn(Opcodes.LCMP)
      case _ => println("visitCmpExpression problem: " + ce)
    }
  }
  
  private def visitTupleExpression(mv: MethodVisitor, te: TupleExpression): Unit = {
    val integers = te.integers
    val size = integers.size
    for(i <- 0 to size - 1){
      val integer = integers(i)
      mv.visitInsn(Opcodes.DUP)
      generateIntConst(mv, i)
      generateIntConst(mv, integer)
      mv.visitInsn(Opcodes.IASTORE)
    }
  }
  
  private def visitBinaryExpression(mv: MethodVisitor, be: BinaryExpression, kind: String, lhsTyp: Option[PrimitiveType]): Unit = {
    visitVarLoad(mv, be.left.varName)
    var rhs1Typ: Option[PrimitiveType] = None
    this.locals(be.left.varName).typ match {
      case pt: PrimitiveType =>
        rhs1Typ = Some(pt)
      case _ =>
    }
    handleTypeImplicitConvert(mv, lhsTyp, rhs1Typ)
    var rhs2Typ: Option[PrimitiveType] = None
    be.right match {
      case Left(va) =>
        visitVarLoad(mv, va.varName)
        this.locals(va.varName).typ match {
          case pt: PrimitiveType =>
            rhs2Typ = Some(pt)
          case _ =>
        }
      case Right(lit) =>
        generateIntConst(mv, lit.text.toInt)
        rhs2Typ = Some(PrimitiveType("int"))
    }
    handleTypeImplicitConvert(mv, lhsTyp, rhs2Typ)
    be.op.text match {
      case "+" => 
        kind match {
          case "int" =>    mv.visitInsn(Opcodes.IADD)
          case "long" =>   mv.visitInsn(Opcodes.LADD)
          case "float" =>  mv.visitInsn(Opcodes.FADD)
          case "double" => mv.visitInsn(Opcodes.DADD)
          case _ =>        println("visitBinaryExpression problem: " + be)
        }
      case "-" => 
        kind match {
          case "int" =>    mv.visitInsn(Opcodes.ISUB)
          case "long" =>   mv.visitInsn(Opcodes.LSUB)
          case "float" =>  mv.visitInsn(Opcodes.FSUB)
          case "double" => mv.visitInsn(Opcodes.DSUB)
          case _ =>        println("visitBinaryExpression problem: " + be)
        }
      case "*" =>
        kind match {
          case "int"  =>   mv.visitInsn(Opcodes.IMUL)
          case "long" =>   mv.visitInsn(Opcodes.LMUL)
          case "float" =>  mv.visitInsn(Opcodes.FMUL)
          case "double" => mv.visitInsn(Opcodes.DMUL)
          case _ =>        println("visitBinaryExpression problem: " + be)
        }
      case "/" =>
        kind match {
          case "int"  =>   mv.visitInsn(Opcodes.IDIV)
          case "long" =>   mv.visitInsn(Opcodes.LDIV)
          case "float" =>  mv.visitInsn(Opcodes.FDIV)
          case "double" => mv.visitInsn(Opcodes.DDIV)
          case _ =>        println("visitBinaryExpression problem: " + be)
        }
      case "%%" =>
        kind match {
          case "int"  =>   mv.visitInsn(Opcodes.IREM)
          case "long" =>   mv.visitInsn(Opcodes.LREM)
          case "float" =>  mv.visitInsn(Opcodes.FREM)
          case "double" => mv.visitInsn(Opcodes.DREM)
          case _ =>        println("visitBinaryExpression problem: " + be)
        }
      case "^&" =>
        kind match {
          case "int"  =>   mv.visitInsn(Opcodes.IAND)
          case "long" =>   mv.visitInsn(Opcodes.LAND)
          case _ =>        println("visitBinaryExpression problem: " + be)
        }
      case "^|" => 
        kind match {
          case "int"  =>   mv.visitInsn(Opcodes.IOR)
          case "long" =>   mv.visitInsn(Opcodes.LOR)
          case _ =>        println("visitBinaryExpression problem: " + be)
        }
      case "^~" => 
        kind match {
          case "int"  =>   mv.visitInsn(Opcodes.IXOR)
          case "long" =>   mv.visitInsn(Opcodes.LXOR)
          case _ =>        println("visitBinaryExpression problem: " + be)
        }
      case "^<" =>
        kind match {
          case "int"  =>   mv.visitInsn(Opcodes.ISHL)
          case "long" =>   mv.visitInsn(Opcodes.LSHL)
          case _ =>        println("visitBinaryExpression problem: " + be)
        }
      case "^>" =>
        kind match {
          case "int"  =>   mv.visitInsn(Opcodes.ISHR)
          case "long" =>   mv.visitInsn(Opcodes.LSHR)
          case _ =>        println("visitBinaryExpression problem: " + be)
        }
      case "^>>" =>
        kind match {
          case "int"  =>   mv.visitInsn(Opcodes.IUSHR)
          case "long" =>   mv.visitInsn(Opcodes.LUSHR)
          case _ =>        println("visitBinaryExpression problem: " + be)
        }
      case _ =>            println("visitBinaryExpression problem: " + be)
    }
  }
  
  private def visitUnaryExpression(mv: MethodVisitor, ue: UnaryExpression): Unit = ue.op.text match {
    case "-" => // Neg int long float double
      visitVarLoad(mv, ue.unary.varName)
      mv.visitInsn(Opcodes.ICONST_M1)
      mv.visitInsn(Opcodes.IMUL)
    case "~" => // Not int long
      visitVarLoad(mv, ue.unary.varName)
      mv.visitInsn(Opcodes.ICONST_M1)
      mv.visitInsn(Opcodes.IXOR)
    case _ =>   println("visitUnaryExpression problem: " + ue)
  }
  
  private def visitLiteralExpression(mv: MethodVisitor, le: LiteralExpression, kind: String): Unit = kind match {
    case "int" => // I
      generateIntConst(mv, le.getInt)
    case "long" => // L
      generateLongConst(mv, le.getLong)
    case "float" => // F
      generateFloatConst(mv, le.getFloat)
    case "double" =>
      generateDoubleConst(mv, le.getDouble)
    case "object" => // String
      visitStringLiteral(mv, le.getString)
    case _ => println("visitLiteralExpression problem: " + kind + " " + le)
  }
  
  private def visitNewExpression(mv: MethodVisitor, ne: NewExpression): Unit = {
    if(ne.typ.isArray){
      ne.typeFragmentsWithInit(0).varNames foreach {
        varName =>
          visitVarLoad(mv, varName)
      }
      JavaKnowledge.isJavaPrimitive(ne.typ.typ) match {
        case true => mv.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_INT)
        case false => mv.visitTypeInsn(Opcodes.ANEWARRAY, getClassName(ne.typ.typ))
      }
    } else {
      mv.visitTypeInsn(Opcodes.NEW, getClassName(ne.typ.name))
    }
  }
  
  private def visitIndexLoad(mv: MethodVisitor, ie: IndexingExpression, kind: String): Unit = {
    visitArrayAccess(mv, ie)
    kind match {
      case "object" => mv.visitInsn(Opcodes.AALOAD)
      case "boolean" => mv.visitInsn(Opcodes.BALOAD)
      case "char" => mv.visitInsn(Opcodes.CALOAD)
      case "double" => mv.visitInsn(Opcodes.DALOAD)
      case "float" => mv.visitInsn(Opcodes.FALOAD)
      case "int" | "byte" | "" => mv.visitInsn(Opcodes.IALOAD)
      case "long" => mv.visitInsn(Opcodes.LALOAD)
      case "short" => mv.visitInsn(Opcodes.SALOAD)
      case _ => println("visitIndexLoad problem: " + kind + " " + ie)
    }
  }
  
  private def visitIndexStore(mv: MethodVisitor, ie: IndexingExpression, kind: String): Unit = {
    kind match {
      case "object" => mv.visitInsn(Opcodes.AASTORE)
      case "boolean" => mv.visitInsn(Opcodes.BASTORE)
      case "char" => mv.visitInsn(Opcodes.CASTORE)
      case "double" => mv.visitInsn(Opcodes.DASTORE)
      case "float" => mv.visitInsn(Opcodes.FASTORE)
      case "int" | "byte" | "" => mv.visitInsn(Opcodes.IASTORE)
      case "long" => mv.visitInsn(Opcodes.LASTORE)
      case "short" => mv.visitInsn(Opcodes.SASTORE)
      case _ => println("visitIndexStore problem: " + kind + " " + ie)
    }
  }
  
  private def visitFieldLoad(mv: MethodVisitor, ae: AccessExpression, typ: JawaType): Unit = {
    visitFieldAccess(mv, ae)
    mv.visitFieldInsn(Opcodes.GETFIELD, ae.fieldSym.baseType.name.replaceAll("\\.", "/"), ae.fieldName, JavaKnowledge.formatTypeToSignature(typ))
  }
  
  private def visitFieldStore(mv: MethodVisitor, ae: AccessExpression, typ: JawaType): Unit = {
    mv.visitFieldInsn(Opcodes.PUTFIELD, ae.fieldSym.baseType.name.replaceAll("\\.", "/"), ae.fieldName, JavaKnowledge.formatTypeToSignature(typ))
  }
  
  private def visitArrayAccess(mv: MethodVisitor, ie: IndexingExpression): Unit = {
    val base: String = ie.base
    val dimentions: Int = ie.dimentions
    val indexs = ie.indices.map(_.index)
    mv.visitVarInsn(Opcodes.ALOAD, this.locals(base).index)
    for(i <- 0 to dimentions - 1){
      val index = indexs(i)
      index match {
        case Left(vs) => 
          visitVarLoad(mv, vs.varName)
        case Right(t) =>
          generateIntConst(mv, t.text.toInt)
      }
    }
    for(i <- 0 to dimentions - 2){
        mv.visitInsn(Opcodes.AALOAD)
    }
  }
  
  private def visitFieldAccess(mv: MethodVisitor, ae: AccessExpression): Unit = {
    val base: String = ae.base
    mv.visitVarInsn(Opcodes.ALOAD, this.locals(base).index)
  }
  
  private def visitVarLoad(mv: MethodVisitor, varName: String): Unit = this.locals(varName).typ.name match {
    case "byte" | "char" | "short" | "int" | "boolean" => 
                     mv.visitVarInsn(Opcodes.ILOAD, this.locals(varName).index)
    case "double" => mv.visitVarInsn(Opcodes.DLOAD, this.locals(varName).index)
    case "float" =>  mv.visitVarInsn(Opcodes.FLOAD, this.locals(varName).index)
    case "long" =>   mv.visitVarInsn(Opcodes.LLOAD, this.locals(varName).index)
    case _ =>        mv.visitVarInsn(Opcodes.ALOAD, this.locals(varName).index)
  }
  
  private def visitVarStore(mv: MethodVisitor, varName: String): Unit = this.locals(varName).typ.name match {
    case "byte" | "char" | "short" | "int" | "boolean" => 
                     mv.visitVarInsn(Opcodes.ISTORE, this.locals(varName).index)
    case "double" => mv.visitVarInsn(Opcodes.DSTORE, this.locals(varName).index)
    case "float" =>  
                     mv.visitVarInsn(Opcodes.FSTORE, this.locals(varName).index)
    case "long" =>   mv.visitVarInsn(Opcodes.LSTORE, this.locals(varName).index)
    case _ =>        mv.visitVarInsn(Opcodes.ASTORE, this.locals(varName).index)
  }
  
  private def visitStringLiteral(mv: MethodVisitor, str: String): Unit = {
    mv.visitLdcInsn(str)
  }
  
  private def visitCastExpression(mv: MethodVisitor, ce: CastExpression, kind: String): Unit = kind match {
    case "i2l" => 
      mv.visitVarInsn(Opcodes.ILOAD, this.locals(ce.varName).index)
      mv.visitInsn(Opcodes.I2L)
    case "i2f" => 
      mv.visitVarInsn(Opcodes.ILOAD, this.locals(ce.varName).index)
      mv.visitInsn(Opcodes.I2F)
    case "i2d" =>
      mv.visitVarInsn(Opcodes.ILOAD, this.locals(ce.varName).index)
      mv.visitInsn(Opcodes.I2D)
    case "l2i" =>
      mv.visitVarInsn(Opcodes.LLOAD, this.locals(ce.varName).index)
      mv.visitInsn(Opcodes.L2I)
    case "l2f" =>
      mv.visitVarInsn(Opcodes.LLOAD, this.locals(ce.varName).index)
      mv.visitInsn(Opcodes.L2F)
    case "l2d" =>
      mv.visitVarInsn(Opcodes.LLOAD, this.locals(ce.varName).index)
      mv.visitInsn(Opcodes.L2D)
    case "f2i" =>
      mv.visitVarInsn(Opcodes.FLOAD, this.locals(ce.varName).index)
      mv.visitInsn(Opcodes.F2I)
    case "f2l" =>
      mv.visitVarInsn(Opcodes.FLOAD, this.locals(ce.varName).index)
      mv.visitInsn(Opcodes.F2L)
    case "f2d" =>
      mv.visitVarInsn(Opcodes.FLOAD, this.locals(ce.varName).index)
      mv.visitInsn(Opcodes.F2D)
    case "d2i" =>
      mv.visitVarInsn(Opcodes.DLOAD, this.locals(ce.varName).index)
      mv.visitInsn(Opcodes.D2I)
    case "d2l" =>
      mv.visitVarInsn(Opcodes.DLOAD, this.locals(ce.varName).index)
      mv.visitInsn(Opcodes.D2L)
    case "d2f" =>
      mv.visitVarInsn(Opcodes.DLOAD, this.locals(ce.varName).index)
      mv.visitInsn(Opcodes.D2F)
    case "i2b" =>
      mv.visitVarInsn(Opcodes.ILOAD, this.locals(ce.varName).index)
      mv.visitInsn(Opcodes.I2B)
    case "i2c" =>
      mv.visitVarInsn(Opcodes.ILOAD, this.locals(ce.varName).index)
      mv.visitInsn(Opcodes.I2C)
    case "i2s" =>
      mv.visitVarInsn(Opcodes.ILOAD, this.locals(ce.varName).index)
      mv.visitInsn(Opcodes.I2S)
    case "object" => 
      mv.visitVarInsn(Opcodes.ALOAD, this.locals(ce.varName).index)
      mv.visitTypeInsn(Opcodes.CHECKCAST, getClassName(ce.typ.typ.name))
    case _ => println("visitCastExpression problem: " + ce + " " + kind)
  }
  
  private def generateIntConst(mv: MethodVisitor, i: Int) = i match {
    case -1 => mv.visitInsn(Opcodes.ICONST_M1)
    case 0  => mv.visitInsn(Opcodes.ICONST_0)
    case 1  => mv.visitInsn(Opcodes.ICONST_1)
    case 2  => mv.visitInsn(Opcodes.ICONST_2)
    case 3  => mv.visitInsn(Opcodes.ICONST_3)
    case 4  => mv.visitInsn(Opcodes.ICONST_4)
    case 5  => mv.visitInsn(Opcodes.ICONST_5)
    case _  =>
      if((i >= Byte.MinValue) && (i <= Byte.MaxValue)) {
        mv.visitIntInsn(Opcodes.BIPUSH, i)
      } else if((i >= Short.MinValue) && (i <= Short.MaxValue)) {
        mv.visitIntInsn(Opcodes.SIPUSH, i)
      } else {
        mv.visitLdcInsn(new Integer(i))
      }
  }
  
  private def generateLongConst(mv: MethodVisitor, l: Long) = l match {
    case 0  => mv.visitInsn(Opcodes.LCONST_0)
    case 1  => mv.visitInsn(Opcodes.LCONST_1)
    case _  =>
      mv.visitLdcInsn(new java.lang.Long(l))
  }
  
  private def generateFloatConst(mv: MethodVisitor, f: Float) = f match {
    case 0  => mv.visitInsn(Opcodes.FCONST_0)
    case 1  => mv.visitInsn(Opcodes.FCONST_1)
    case 2  => mv.visitInsn(Opcodes.FCONST_2)
    case _  =>
      mv.visitLdcInsn(new java.lang.Float(f))
  }
  
  private def generateDoubleConst(mv: MethodVisitor, d: Double) = d match {
    case 0  => mv.visitInsn(Opcodes.DCONST_0)
    case 1  => mv.visitInsn(Opcodes.DCONST_1)
    case _  =>
      mv.visitLdcInsn(new java.lang.Double(d))
  }
}