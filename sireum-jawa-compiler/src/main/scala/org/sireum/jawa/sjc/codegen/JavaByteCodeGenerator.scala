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

class JavaByteCodeGenerator {
  private val classes: MMap[String, Array[Byte]] = mmapEmpty
  
  def getClasses: IMap[String, Array[Byte]] = classes.toMap
  
  def generate(cu: JawaCompilationUnit): Unit = {
    cu.topDecls foreach {
      cid =>
        visitClass(cid, Opcodes.V1_8)
    }
  }
  
  private def getClassName(name: String): String = {
    name.replaceAll(".", "/")
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
    cw.visit(javaVersion, mod, null, getClassName(cid.typ.name), superName, interfaceNames.toArray)
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
        mv.visitLabel(locLabel)
        this.locations(location.locationUri) = locLabel
    }
    body.locations foreach {
      location =>
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
      
      case _ =>
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
      val arg = cs.argClause.arg(i)
      val typ = cs.signature.getParameterTypes()(i) match {
        case o: ObjectType => "object"
        case t: JawaType => t.name
      }
      visitVarLoad(mv, arg, typ)
    }
        
    val opcode = 
      if(cs.isDirect || cs.isVirtual) Opcodes.INVOKEVIRTUAL
      else if(cs.isStatic) Opcodes.INVOKESTATIC
      else if(cs.isSuper) Opcodes.INVOKESPECIAL
      else if(cs.isInterface) Opcodes.INVOKEINTERFACE
      else Opcodes.INVOKEVIRTUAL
    val className: String = JavaKnowledge.formatTypeToSignature(cs.signature.getClassType)
    val methodName: String = cs.signature.methodNamePart
    val descriptor: String = cs.signature.getDescriptor
    val ltf = opcode == Opcodes.INVOKEINTERFACE
    mv.visitMethodInsn(opcode, className, methodName, descriptor, ltf)
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
   * lit: lit16, lit8
   * cast: i2l, i2f, i2d, l2i, l2f, l2d, f2i, f2l, f2d, d2i, d2l, d2f, i2b, i2c, i2s, object
   */
  private def visitAssignmentStatement(mv: MethodVisitor, as: AssignmentStatement, typ: String): Unit = {
    visitRhsExpression(mv, as.rhs, typ)
    
  }
  
  private def visitRhsExpression(mv: MethodVisitor, exp: Expression, typ: String): Unit = exp match {
    case ne: NameExpression =>
      ne.varSymbol match {
        case Left(varSym) =>
          visitVarLoad(mv, ne.name, typ)
        case Right(fnSym) =>
          mv.visitFieldInsn(Opcodes.GETSTATIC, fnSym.baseType.name.replaceAll(".", "/"), fnSym.fieldName, null /*TODO*/)
      }
    case ie: IndexingExpression =>
      visitIndexLoad(mv, ie, typ)
    case ae: AccessExpression =>
      visitFieldLoad(mv, ae, typ)
    case te: TupleExpression =>
      /*TODO*/
    case ce: CastExpression =>
      visitCastExpression(mv, ce, typ)
    case ce: NewExpression =>
    case le: LiteralExpression =>
    case ue: UnaryExpression =>
    case be: BinaryExpression =>
    case ce: CmpExpression =>
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
  
  private def visitFieldLoad(mv: MethodVisitor, ae: AccessExpression, typ: String): Unit = {
    visitFieldAccess(mv, ae)
    mv.visitFieldInsn(Opcodes.GETFIELD, ae.fieldSym.baseType.name.replaceAll(".", "/"), ae.fieldName, null /*TODO*/)
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
          visitNumberLiteral(mv, t.text.toInt)
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
    case "byte" | "char" | "short" | "int" | "boolean" => 
                     mv.visitVarInsn(Opcodes.ILOAD, this.locals(varName).index)
    case "double" => mv.visitVarInsn(Opcodes.DLOAD, this.locals(varName).index)
    case "float" =>  mv.visitVarInsn(Opcodes.FLOAD, this.locals(varName).index)
    case "long" =>   mv.visitVarInsn(Opcodes.LLOAD, this.locals(varName).index)
    case "wide" =>   mv.visitVarInsn(Opcodes.DLOAD, this.locals(varName).index)
    case _ =>        println("Why it comes here: " + varName + " " + typ)
  }
  
  private def visitStringLiteral(mv: MethodVisitor, str: String): Unit = {
    mv.visitLdcInsn(str)
  }
  
  private def visitNumberLiteral(mv: MethodVisitor, i: Int): Unit = {
    generateIntConst(mv, i)
  }
  
  private def visitCastExpression(mv: MethodVisitor, ce: CastExpression, typ: String): Unit = typ match {
    case "i2l" => //mv.visitInsn(arg0)
    case "i2f" => 
    case "i2d" =>
    case "l2i" =>
    case "l2f" =>
    case "l2d" =>
    case "f2i" =>
    case "f2l" =>
    case "f2d" =>
    case "d2i" =>
    case "d2l" =>
    case "d2f" =>
    case "i2b" =>
    case "i2c" =>
    case "i2s" =>
    case "object" => 
    case _ =>       // println("Why it comes here: " + varName + " " + typ)
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
}