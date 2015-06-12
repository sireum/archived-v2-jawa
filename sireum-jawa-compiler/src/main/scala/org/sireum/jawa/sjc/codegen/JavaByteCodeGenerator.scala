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

object JavaByteCodeGenerator {
  def outputByteCodes(pw: PrintWriter, bytecodes: Array[Byte]) = {
    val cr = new ClassReader(bytecodes)
    val tcv = new TraceClassVisitor(pw)
    cr.accept(tcv, ClassReader.SKIP_FRAMES)
    pw.flush()
  }
}

class JavaByteCodeGenerator {
  private val classes: MMap[String, Array[Byte]] = mmapEmpty
  
  def getClasses: IMap[String, Array[Byte]] = classes.toMap
  
  def generate(cu: JawaCompilationUnit): IMap[String, Array[Byte]] = {
    cu.topDecls foreach {
      cid =>
        visitClass(cid, Opcodes.V1_8)
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
    else
      mod = mod | Opcodes.ACC_PUBLIC
    if(AccessFlag.isAbstract(af))
      mod = mod | Opcodes.ACC_ABSTRACT
    if(AccessFlag.isFinal(af))
      mod = mod | Opcodes.ACC_FINAL
    if(AccessFlag.isStatic(af))
      mod = mod | Opcodes.ACC_STATIC
    if(AccessFlag.isInterface(af))
      mod = mod | Opcodes.ACC_INTERFACE
    mod
  }
  
  private def visitClass(cid: ClassOrInterfaceDeclaration, javaVersion: Int): Unit = {
    val cw: ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
    val af: Int = AccessFlag.getAccessFlags(cid.accessModifier)
    val mod = getJavaFlags(af)
    
    val superName: String = cid.superClassOpt match {
      case Some(su) => getClassName(su.name)
      case None => if(cid.typ.name != JavaKnowledge.JAVA_TOPLEVEL_OBJECT) getClassName(JavaKnowledge.JAVA_TOPLEVEL_OBJECT) else null
    }
    val interfaceNames: IList[String] = cid.interfaces map(i => getClassName(i.name))
    cw.visit(javaVersion, mod, getClassName(cid.typ.name), null, superName, interfaceNames.toArray)
    cw.visitSource(null, null)
    cid.fields foreach {
      fd => visitField(cw, fd)
    }
    cid.methods foreach {
      md => visitMethod(cw, md)
    }
    cw.visitEnd()
    this.classes(cid.typ.name) = cw.toByteArray()
  }
  
  private def visitField(cw: ClassWriter, fd: JawaField with JawaDeclaration): Unit = {
    val af: Int = AccessFlag.getAccessFlags(fd.accessModifier)
    val mod: Int = getJavaFlags(af)
    val typ: String = JavaKnowledge.formatTypeToSignature(fd.typ.typ)
    cw.visitField(mod, fd.fieldName, typ, null, null).visitEnd()
  }
  
  case class LocalIndex(varname: String, typ: String, index: Int)
  private val locals: MMap[String, LocalIndex] = mmapEmpty
  private val locations: MMap[String, Label] = mmapEmpty
  
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
        locals(t.name) = LocalIndex(t.name, JavaKnowledge.formatTypeToSignature(t.typ.typ), i)
        i += 1
    }
    md.paramlist.foreach{
      param =>
        locals(param.name) = LocalIndex(param.name, JavaKnowledge.formatTypeToSignature(param.typ.typ), i)
        i += 1
    }
    body.locals.foreach{
      local =>
        locals(local.varSymbol.varName) = LocalIndex(local.varSymbol.varName, JavaKnowledge.formatTypeToSignature(JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE), i)
        i += 1
    }
    val initLabel = new Label()
    mv.visitCode()
    mv.visitLabel(initLabel)
    
    body.locations foreach {
      location =>
        val locLabel = new Label()
        this.locations(location.locationUri) = locLabel
    }
    body.locations foreach {
      location =>
        val locLabel = this.locations(location.locationUri)
        mv.visitLabel(locLabel)
        visitLocation(mv, location)
    }
    
    val endLabel = new Label()
    mv.visitLabel(endLabel)
    this.locals foreach {
      case (name, local) =>
        mv.visitLocalVariable(local.varname, local.typ, null, initLabel, endLabel, local.index)
    }
    mv.visitMaxs(0, 0)
    mv.visitEnd()
    this.locals.clear()
    this.locations.clear()
  }
  
  private def visitLocation(mv: MethodVisitor, jl: JawaLocation): Unit = {
    jl.statement match {
      case cs: CallStatement =>
        visitCallStatement(mv, cs)
      case as: AssignmentStatement =>
        val typ: String = as.typ
        visitAssignmentStatement(mv, as, typ)
      case ts: ThrowStatement =>
      case is: IfStatement =>
        visitIfStatement(mv, is)
      case gs: GotoStatement =>
        visitGotoStatement(mv, gs)
      case ss: SwitchStatement =>
      case rs: ReturnStatement =>
        val typ: String = rs.typ
        visitReturnStatement(mv, rs, typ)
      case es: EmptyStatement =>
      case _ =>
    }
  }
  
  private def visitGotoStatement(mv: MethodVisitor, gs: GotoStatement): Unit = {
    val target = this.locations(gs.targetLocation.location)
    mv.visitJumpInsn(Opcodes.GOTO, target)
  }
  
  private def visitIfStatement(mv: MethodVisitor, is: IfStatement): Unit = {
    val left = is.cond.left.varName
    visitVarLoad(mv, left, "int")
    is.cond.right match {
      case Left(right) => visitVarLoad(mv, right.varName, "int")
      case Right(right) => generateIntConst(mv, right.text.toInt)
    }
    val target = this.locations(is.targetLocation.location)
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
  
  private def visitReturnStatement(mv: MethodVisitor, rs: ReturnStatement, typ: String): Unit = {
    rs.varOpt match {
      case Some(va) => 
        visitVarLoad(mv, va.varName, typ)
        typ match {
          case "" => mv.visitInsn(Opcodes.IRETURN)
          case "wide" => mv.visitInsn(Opcodes.DRETURN)
          case "object" => mv.visitInsn(Opcodes.ARETURN)
//          case "double" =>
//          case "float" =>
//          case "int" =>
//          case "long" =>
          case _ => println("visitReturnStatement problem: " + rs + " " + typ)
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
      val typ = cs.signature.getParameterTypes()(i) match {
        case o: ObjectType => "object"
        case t: JawaType => t.name
      }
      visitVarLoad(mv, arg, typ)
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
      case typ => visitVarStore(mv, cs.lhs.varName, typ)
    }
  }
  
  /**
   * typ could be:
   * move: "", wide, object
   * return: "", wide, object, Void
   * aget: int, wide, object, boolean, byte, char, short
   * aput: int, wide, object, boolean, byte, char, short
   * iget: int, wide, object, boolean, byte, char, short, iget_quick, iget_wide_quick, iget_object_quick
   * iput: int, wide, object, boolean, byte, char, short, iget_quick, iget_wide_quick, iget_object_quick
   * sget: int, wide, object, boolean, byte, char, short
   * sput: int, wide, object, boolean, byte, char, short
   * unop: int, long, float, double
   * biop: int, long, float, double
   * const: const4, const16, const32, high16, wide16, wide32, wide_high16, wide
   * lit: lit16, lit8
   * cast: i2l, i2f, i2d, l2i, l2f, l2d, f2i, f2l, f2d, d2i, d2l, d2f, i2b, i2c, i2s, object
   */
  private def visitAssignmentStatement(mv: MethodVisitor, as: AssignmentStatement, typ: String): Unit = {
    val lhs = as.lhs
    val rhs = as.rhs
    
    lhs match {
      case ie: IndexingExpression =>
        visitArrayAccess(mv, ie)
      case ae: AccessExpression =>
        visitFieldAccess(mv, ae)
      case _ =>
    }
    
    visitRhsExpression(mv, rhs, typ)
    visitLhsExpression(mv, lhs, typ)
  }
  
  private def visitLhsExpression(mv: MethodVisitor, lhs: Expression with LHS, typ: String): Unit = lhs match {
    case ne: NameExpression =>
      ne.varSymbol match {
        case Left(varSym) =>
          visitVarStore(mv, ne.name, typ)
        case Right(fnSym) =>
          val t = JavaKnowledge.getTypeFromName(typ)
          mv.visitFieldInsn(Opcodes.PUTSTATIC, fnSym.baseType.name.replaceAll("\\.", "/"), fnSym.fieldName, JavaKnowledge.formatTypeToSignature(t))
      }
    case ie: IndexingExpression =>
      visitIndexStore(mv, ie, typ)
    case ae: AccessExpression =>
      visitFieldStore(mv, ae, typ)
    case _ => println("visitLhsExpression problem: " + lhs + " " + typ)
  }
  
  private def visitRhsExpression(mv: MethodVisitor, rhs: Expression with RHS, typ: String): Unit = rhs match {
    case ne: NameExpression =>
      ne.varSymbol match {
        case Left(varSym) =>
          visitVarLoad(mv, ne.name, typ)
        case Right(fnSym) =>
          val t = JavaKnowledge.getTypeFromName(typ)
          mv.visitFieldInsn(Opcodes.GETSTATIC, fnSym.baseType.name.replaceAll("\\.", "/"), fnSym.fieldName, JavaKnowledge.formatTypeToSignature(t))
      }
    case ie: IndexingExpression =>
      visitIndexLoad(mv, ie, typ)
    case ae: AccessExpression =>
      visitFieldLoad(mv, ae, typ)
    case te: TupleExpression =>
      /*TODO*/
    case ce: CastExpression =>
      visitCastExpression(mv, ce, typ)
    case ne: NewExpression =>
      visitNewExpression(mv, ne)
    case le: LiteralExpression =>
      visitLiteralExpression(mv, le, typ)
    case ue: UnaryExpression =>
      visitUnaryExpression(mv, ue, typ)
    case be: BinaryExpression =>
      visitBinaryExpression(mv, be, typ)
    case ce: CmpExpression =>
      /*TODO*/
    case _ =>  println("visitRhsExpression problem: " + rhs + " " + typ)
  }
  
  private def visitBinaryExpression(mv: MethodVisitor, be: BinaryExpression, typ: String): Unit = {
    visitVarLoad(mv, be.left.varName, typ)
    be.right match {
      case Left(va) =>
        visitVarLoad(mv, va.varName, typ)
      case Right(lit) =>
        generateIntConst(mv, lit.text.toInt)
    }
    be.op.text match {
      case "+" => 
        typ match {
          case "int" | "lit16" | "lit8" => mv.visitInsn(Opcodes.IADD)
          case "long" =>                   mv.visitInsn(Opcodes.LADD)
          case "float" =>                  mv.visitInsn(Opcodes.FADD)
          case "double" =>                 mv.visitInsn(Opcodes.DADD)
          case _ =>                        println("visitBinaryExpression problem: " + be)
        }
      case "-" => 
        typ match {
          case "int" | "lit16" | "lit8" => mv.visitInsn(Opcodes.ISUB)
          case "long" =>                   mv.visitInsn(Opcodes.LSUB)
          case "float" =>                  mv.visitInsn(Opcodes.FSUB)
          case "double" =>                 mv.visitInsn(Opcodes.DSUB)
          case _ =>                        println("visitBinaryExpression problem: " + be)
        }
      case "*" =>
        typ match {
          case "int" | "lit16" | "lit8" => mv.visitInsn(Opcodes.IMUL)
          case "long" =>                   mv.visitInsn(Opcodes.LMUL)
          case "float" =>                  mv.visitInsn(Opcodes.FMUL)
          case "double" =>                 mv.visitInsn(Opcodes.DMUL)
          case _ =>                        println("visitBinaryExpression problem: " + be)
        }
      case "/" =>
        typ match {
          case "int" | "lit16" | "lit8" => mv.visitInsn(Opcodes.IDIV)
          case "long" =>                   mv.visitInsn(Opcodes.LDIV)
          case "float" =>                  mv.visitInsn(Opcodes.FDIV)
          case "double" =>                 mv.visitInsn(Opcodes.DDIV)
          case _ =>                        println("visitBinaryExpression problem: " + be)
        }
      case "%%" =>
        typ match {
          case "int" | "lit16" | "lit8" => mv.visitInsn(Opcodes.IREM)
          case "long" =>                   mv.visitInsn(Opcodes.LREM)
          case "float" =>                  mv.visitInsn(Opcodes.FREM)
          case "double" =>                 mv.visitInsn(Opcodes.DREM)
          case _ =>                        println("visitBinaryExpression problem: " + be)
        }
      case "^&" =>
        typ match {
          case "int" | "lit16" | "lit8" => mv.visitInsn(Opcodes.IAND)
          case "long" =>                   mv.visitInsn(Opcodes.LAND)
          case _ =>                        println("visitBinaryExpression problem: " + be)
        }
      case "^|" => 
        typ match {
          case "int" | "lit16" | "lit8" => mv.visitInsn(Opcodes.IOR)
          case "long" =>                   mv.visitInsn(Opcodes.LOR)
          case _ =>                        println("visitBinaryExpression problem: " + be)
        }
      case "^~" => 
        typ match {
          case "int" | "lit16" | "lit8" => mv.visitInsn(Opcodes.IXOR)
          case "long" =>                   mv.visitInsn(Opcodes.LXOR)
          case _ =>                        println("visitBinaryExpression problem: " + be)
        }
      case "^<" =>
        typ match {
          case "int" | "lit16" | "lit8" => mv.visitInsn(Opcodes.ISHL)
          case "long" =>                   mv.visitInsn(Opcodes.LSHL)
          case _ =>                        println("visitBinaryExpression problem: " + be)
        }
      case "^>" =>
        typ match {
          case "int" | "lit16" | "lit8" => mv.visitInsn(Opcodes.ISHR)
          case "long" =>                   mv.visitInsn(Opcodes.LSHR)
          case _ =>                        println("visitBinaryExpression problem: " + be)
        }
      case "^>>" =>
        typ match {
          case "int" | "lit16" | "lit8" => mv.visitInsn(Opcodes.IUSHR)
          case "long" =>                   mv.visitInsn(Opcodes.LUSHR)
          case _ =>                        println("visitBinaryExpression problem: " + be)
        }
      case _ =>                            println("visitBinaryExpression problem: " + be)
    }
  }
  
  private def visitUnaryExpression(mv: MethodVisitor, ue: UnaryExpression, typ: String): Unit = ue.op.text match {
    case "-" => // Neg int long float double
      visitVarLoad(mv, ue.unary.varName, typ)
      mv.visitInsn(Opcodes.ICONST_M1)
      mv.visitInsn(Opcodes.IMUL)
    case "~" => // Not int long
      visitVarLoad(mv, ue.unary.varName, typ)
      mv.visitInsn(Opcodes.ICONST_M1)
      mv.visitInsn(Opcodes.IXOR)
    case _ =>   println("visitUnaryExpression problem: " + ue)
  }
  
  private def visitLiteralExpression(mv: MethodVisitor, le: LiteralExpression, typ: String): Unit = typ match {
    case "const4" | "const16" | "const32" |
         "high16" | "wide" => // I
      generateIntConst(mv, le.getInt)
    case "wide16" | "wide_high16" => // L
      generateLongConst(mv, le.getLong)
    case "wide32" | "wide" => // F
      generateFloatConst(mv, le.getFloat)
    case "object" => // String
      visitStringLiteral(mv, le.getString)
    case _ => println("visitLiteralExpression problem: " + typ + " " + le)
  }
  
  private def visitNewExpression(mv: MethodVisitor, ne: NewExpression): Unit = {
    if(ne.getType.isArray){
      ne.typ.typeFragments(0).asInstanceOf[TypeFragmentWithInit].varNames foreach {
        varName =>
          visitVarLoad(mv, varName, "int")
      }
      JavaKnowledge.isJavaPrimitive(ne.getType.typ) match {
        case true => mv.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_INT)
        case false => mv.visitTypeInsn(Opcodes.ANEWARRAY, getClassName(ne.getType.typ))
      }
    } else {
      mv.visitTypeInsn(Opcodes.NEW, getClassName(ne.getType.name))
    }
  }
  
  private def visitIndexLoad(mv: MethodVisitor, ie: IndexingExpression, typ: String): Unit = {
    visitArrayAccess(mv, ie)
    typ match {
      case "object" => mv.visitInsn(Opcodes.AALOAD)
      case "boolean" => mv.visitInsn(Opcodes.BALOAD)
      case "char" => mv.visitInsn(Opcodes.CALOAD)
      case "double" => mv.visitInsn(Opcodes.DALOAD)
      case "float" => mv.visitInsn(Opcodes.FALOAD)
      case "int" => mv.visitInsn(Opcodes.IALOAD)
      case "long" => mv.visitInsn(Opcodes.LALOAD)
      case "short" => mv.visitInsn(Opcodes.SALOAD)
      case _ => println("visitIndexLoad problem: " + typ + " " + ie)
    }
  }
  
  private def visitIndexStore(mv: MethodVisitor, ie: IndexingExpression, typ: String): Unit = {
    typ match {
      case "object" => mv.visitInsn(Opcodes.AASTORE)
      case "boolean" => mv.visitInsn(Opcodes.BASTORE)
      case "char" => mv.visitInsn(Opcodes.CASTORE)
      case "double" => mv.visitInsn(Opcodes.DASTORE)
      case "float" => mv.visitInsn(Opcodes.FASTORE)
      case "int" | "" => mv.visitInsn(Opcodes.IASTORE)
      case "long" => mv.visitInsn(Opcodes.LASTORE)
      case "short" => mv.visitInsn(Opcodes.SASTORE)
      case _ => println("visitIndexStore problem: " + typ + " " + ie)
    }
  }
  
  private def visitFieldLoad(mv: MethodVisitor, ae: AccessExpression, typ: String): Unit = {
    visitFieldAccess(mv, ae)
    val t = JavaKnowledge.getTypeFromName(typ)
    mv.visitFieldInsn(Opcodes.GETFIELD, ae.fieldSym.baseType.name.replaceAll("\\.", "/"), ae.fieldName, JavaKnowledge.formatTypeToSignature(t))
  }
  
  private def visitFieldStore(mv: MethodVisitor, ae: AccessExpression, typ: String): Unit = {
    val t = JavaKnowledge.getTypeFromName(typ)
    mv.visitFieldInsn(Opcodes.PUTFIELD, ae.fieldSym.baseType.name.replaceAll("\\.", "/"), ae.fieldName, JavaKnowledge.formatTypeToSignature(t))
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
          visitVarLoad(mv, vs.varName, "int")
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
  
  private def visitVarLoad(mv: MethodVisitor, varName: String, typ: String): Unit = typ match {
    case "object" => mv.visitVarInsn(Opcodes.ALOAD, this.locals(varName).index)
    case "byte" | "char" | "short" | "int" | "boolean" | "" => 
                     mv.visitVarInsn(Opcodes.ILOAD, this.locals(varName).index)
    case "double" => mv.visitVarInsn(Opcodes.DLOAD, this.locals(varName).index)
    case "float" =>  mv.visitVarInsn(Opcodes.FLOAD, this.locals(varName).index)
    case "long" =>   mv.visitVarInsn(Opcodes.LLOAD, this.locals(varName).index)
    case "wide" =>   mv.visitVarInsn(Opcodes.DLOAD, this.locals(varName).index)
    case _ =>        println("visitVarLoad problem: " + varName + " " + typ)
  }
  
  private def visitVarStore(mv: MethodVisitor, varName: String, typ: String): Unit = typ match {
    case "object" => mv.visitVarInsn(Opcodes.ASTORE, this.locals(varName).index)
    case "byte" | "char" | "short" | "int" | "boolean" | "" => 
                     mv.visitVarInsn(Opcodes.ISTORE, this.locals(varName).index)
    case "double" => mv.visitVarInsn(Opcodes.DSTORE, this.locals(varName).index)
    case "float" =>  mv.visitVarInsn(Opcodes.FSTORE, this.locals(varName).index)
    case "long" =>   mv.visitVarInsn(Opcodes.LSTORE, this.locals(varName).index)
    case "wide" =>   mv.visitVarInsn(Opcodes.DSTORE, this.locals(varName).index)
    case "const4" | "const16" | "const32" | "high16" =>
                     mv.visitVarInsn(Opcodes.ISTORE, this.locals(varName).index)
    case "wide16" | "wide_high16" =>
                     mv.visitVarInsn(Opcodes.LSTORE, this.locals(varName).index)
    case "wide" | "wide32" => 
                     mv.visitVarInsn(Opcodes.FSTORE, this.locals(varName).index)
    case _ =>        println("visitVarStore problem: " + varName + " " + typ)
  }
  
  private def visitStringLiteral(mv: MethodVisitor, str: String): Unit = {
    mv.visitLdcInsn(str)
  }
  
  private def visitCastExpression(mv: MethodVisitor, ce: CastExpression, typ: String): Unit = typ match {
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
    case _ => println("visitCastExpression problem: " + ce + " " + typ)
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
}