package org.sireum.jawa.sjc.refactoring

import org.sireum.jawa.DefaultReporter
import org.sireum.util._
import org.sireum.alir.AlirLocationNode
import org.sireum.jawa.sjc.parser._
import org.sireum.jawa.JawaType
import org.sireum.alir.AlirVirtualNode
import org.sireum.jawa.ObjectType
import org.sireum.jawa.JavaKnowledge
import org.sireum.jawa.sjc.alir.ControlFlowGraph
import org.sireum.alir.AlirIntraProceduralGraph
import org.sireum.jawa.io.Position
import org.sireum.jawa.PrimitiveType

/**
 * @author fgwei
 */
object RefactorJawa {
  def apply(code: String): String = {
    val newcode = resolveCallStatement(code)
    val sb: StringBuilder = new StringBuilder
    val reporter = new DefaultReporter
    val cuOpt: Option[CompilationUnit] = JawaParser.parse[CompilationUnit](Left(newcode), true, reporter)
    cuOpt match {
      case Some(cu) =>
        cu.topDecls foreach {
          c =>
            val classcode = c.toCode
            val head = if(c.methods.size > 0) code.substring(0, c.methods(0).firstToken.pos.start - c.methods(0).firstToken.pos.column) else code
            sb.append(head)
            c.methods foreach {
              md =>
                val pool: AlirIntraProceduralGraph.NodePool = mmapEmpty
                val cfg = ControlFlowGraph[String](md, "Entry", "Exit", pool, ControlFlowGraph.defaultSiff)
                val methodcode = resolveLocalVarType(md, cfg)
                sb.append(methodcode + "\n")
            }
        }
      case None =>
        println(reporter.problems)
    }
    val finalcode = sb.toString().trim()
    finalcode
  }
  
  def resolveLocalVarType(md: MethodDeclaration, cfg: ControlFlowGraph[String]): String = {
    val localvars: MMap[String, (JawaType, Boolean)] = mmapEmpty // map from variable -> (typ, isParam)
    val locations: MMap[Int, String] = mmapEmpty // map from index -> location string
    val recentvars: MMap[String, String] = mmapEmpty // map from var -> newvar
    val pendingTasks: MMap[String, (String, Int, Position, String)] = mmapEmpty
    
    def handlePendingTask(varname: String, typ: JawaType): Unit = {
      pendingTasks.get(varname) match {
        case Some((l, i, pos, v)) =>
          var newvar = typ.typ.substring(typ.typ.lastIndexOf(".") + 1) + {if(typ.dimensions > 0)"_arr" + typ.dimensions else ""} + "_" + v
          localvars(newvar) = ((typ, false))
          recentvars(v) = newvar
          var newl = updateCode(l, pos, newvar)
          if(typ.isInstanceOf[ObjectType]) newl = newl.replace("0I  @kind int", "null  @kind object")
          locations(i) = newl
        case None =>
      }
      pendingTasks.remove(varname)
    }
    
    
//    val updateEvidence: MMap[Position, String] = mmapEmpty // map from pos -> new string
    val resolved: MSet[ControlFlowGraph.Node] = msetEmpty
    
    val sb: StringBuilder = new StringBuilder
    val code = md.toCode
    var head: String = code.substring(0, code.indexOf("{") + 1)
    val sig = md.signature
    val params = md.paramlist 
    val types = sig.getParameterTypes()
    
    for(i <- 1 to params.size) {
      val param = params(params.size - i)
      val typ = types(params.size - i)
      var newvar = typ.typ.substring(typ.typ.lastIndexOf(".") + 1) + {if(typ.dimensions > 0)"_arr" + typ.dimensions else ""} + "_" + param.name
      if(localvars.contains(newvar) && localvars(newvar)._1 != typ) newvar = "a" + newvar
      localvars(newvar) = ((typ, true))
      recentvars(param.name) = newvar
      head = updateCode(head, param.paramSymbol.id.pos, newvar)
//      updateEvidence(param.paramSymbol.id.pos) = newvar
    }
    
    md.thisParam match {
      case Some(t) =>
        val newvar = "this_" + t.name
        localvars(newvar) = ((md.enclosingTopLevelClass.typ, true))
        recentvars(t.name) = newvar
        head = updateCode(head, t.paramSymbol.id.pos, newvar)
//        updateEvidence(t.paramSymbol.id.pos) = newvar
      case None =>
    }
    
    sb.append(head + "\n")
    val body: ResolvedBody = md.body match {
      case rb: ResolvedBody => rb
      case ub: UnresolvedBody => ub.resolve
    }
    val entry: ControlFlowGraph.Node = cfg.getVirtualNode("Entry")
    val worklist: MList[ControlFlowGraph.Node] = mlistEmpty
    worklist ++= cfg.successors(entry)
    
    while(!worklist.isEmpty){
      val n = worklist.remove(0)
      resolved += n
      worklist ++= cfg.successors(n).filter(s => !resolved.contains(s))
      n match {
        case alun: AlirLocationNode => 
          val index: Int = alun.locIndex
          val loc = body.locations(index)
          var locCode = loc.toCode
          var possibleNull: Boolean = false
          loc.statement match {
            case cs: CallStatement =>
              val paramTypes = cs.signature.getParameterTypes()
              val args = cs.argVars
              val size = paramTypes.size
              for(i <- 1 to size) {
                val arg = args(size - i)
                val typ = paramTypes(size - i)
                handlePendingTask(arg.varName, typ)
                val newarg = recentvars(arg.varName)
                locCode = updateCode(locCode, arg.id.pos, newarg)
//                  updateEvidence(arg._1.id.pos) = newarg
              }
              cs.recvVarOpt match {
                case Some(recv) =>
                  val typ = cs.signature.getClassType
                  handlePendingTask(recv.varName, typ)
                  val newarg = recentvars(recv.varName)
                  locCode = updateCode(locCode, recv.id.pos, newarg)
                case None =>
              }
              cs.lhsOpt match {
                case Some(lhs) =>
                  val retType = cs.signature.getReturnType()
                  var newvar = retType.typ.substring(retType.typ.lastIndexOf(".") + 1) + {if(retType.dimensions > 0)"_arr" + retType.dimensions else ""} + "_" + lhs.lhs.varName
                  if(localvars.contains(newvar) && localvars(newvar)._1 != retType) newvar = "a" + newvar
                  localvars(newvar) = ((cs.signature.getReturnType(), false))
                  recentvars(lhs.lhs.varName) = newvar
                  locCode = updateCode(locCode, lhs.lhs.id.pos, newvar)
//                  updateEvidence(lhs.lhs.id.pos) = newvar
                case None =>
              }
            case as: AssignmentStatement =>
              val typOpt: Option[JawaType] = as.typOpt
              val kind: String = as.kind
              var rhsType: JawaType = null
              as.rhs match {
                case ne: NameExpression =>
                  ne.varSymbol match {
                    case Left(v) =>
                      val varname = v.varName
                      val typ: JawaType = typOpt match {
                        case Some(t) => t
                        case None => 
                          kind match {
                            case "object" => JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE
                            case a => JawaType.generateType(a, 0)
                          }
                      }
                      handlePendingTask(varname, typ)
                      val newarg = recentvars(varname)
                      locCode = updateCode(locCode, v.id.pos, newarg)
//                      updateEvidence(v.id.pos) = newarg
                      rhsType = localvars(newarg)._1
                    case Right(f) =>
                      rhsType = typOpt.get
                  }
                case ee: ExceptionExpression =>
                  rhsType = new ObjectType("java.lang.Throwable")/*TODO*/
                case ie: IndexingExpression =>
                  ie.indices.reverse.foreach{
                    indice =>
                      indice.index match {
                        case Left(v) =>
                          val varName = v.varName
                          handlePendingTask(varName, PrimitiveType("int"))
                          val newarg = recentvars(varName)
                          locCode = updateCode(locCode, v.id.pos, newarg)
                        case Right(c) =>
                      }
                  }
                  val varname = ie.base
                  handlePendingTask(varname, JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE) /*TODO need to think whether its possible to refer the type*/
                  val newarg = recentvars(varname)
                  locCode = updateCode(locCode, ie.varSymbol.id.pos, newarg)
//                  updateEvidence(ie.varSymbol.id.pos) = newarg
                  val dimentions = ie.dimentions
                  val typ = localvars(newarg)._1.asInstanceOf[ObjectType]
                  rhsType = JawaType.generateType(typ.typ, typ.dimensions - dimentions)
                case ae: AccessExpression =>
                  val varname = ae.base
                  handlePendingTask(varname, typOpt.get)
                  val newarg = recentvars(varname)
                  locCode = updateCode(locCode, ae.varSymbol.id.pos, newarg)
//                  updateEvidence(ae.varSymbol.id.pos) = newarg
                  rhsType = typOpt.get
                case te: TupleExpression =>
                  rhsType = ObjectType("char", 1) /*TODO*/
                case ce: CastExpression =>
                  val varname = ce.varName
                  handlePendingTask(varname, JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE)
                  val newarg = recentvars(varname)
                  locCode = updateCode(locCode, ce.varSym.id.pos, newarg)
//                  updateEvidence(ce.varSym.id.pos) = newarg
                  rhsType = ce.typ.typ
                case ne: NewExpression =>
                  ne.typeFragmentsWithInit.reverse.foreach{
                    tf =>
                      tf.varSymbols.reverse.foreach{
                        v =>
                          val varname = v._1.varName
                          handlePendingTask(varname, PrimitiveType("int"))
                          val newarg = recentvars(varname)
                          locCode = updateCode(locCode, v._1.id.pos, newarg)
                      }
                  }
                  rhsType = ne.typ
                case le: LiteralExpression =>
                  import org.sireum.jawa.sjc.lexer.Tokens._
                  le.constant.tokenType match {
                    case STRING_LITERAL =>
                      rhsType = new ObjectType("java.lang.String")
                    case FLOATING_POINT_LITERAL =>
                      rhsType = PrimitiveType(kind)
                    case INTEGER_LITERAL =>
                      if(kind == "int" && le.getInt == 0){ // it is possible as object null
                        possibleNull = true
                      }
                      rhsType = PrimitiveType(kind)
                    case CHARACTER_LITERAL =>
                      rhsType = PrimitiveType(kind)
                  }
                case ue: UnaryExpression =>
                  val varname = ue.unary.varName
                  handlePendingTask(varname, PrimitiveType(kind))
                  val newarg = recentvars(varname)
                  locCode = updateCode(locCode, ue.unary.id.pos, newarg)
//                  updateEvidence(ue.unary.id.pos) = newarg
                  rhsType = PrimitiveType(kind)
                case be: BinaryExpression =>
                  val rightname = be.right match {
                    case Left(v) =>
                      val rightname = v.varName
                      handlePendingTask(rightname, PrimitiveType(kind))
                      val newright = recentvars(rightname)
                      locCode = updateCode(locCode, v.id.pos, newright)
//                      updateEvidence(v.id.pos) = newright
                    case Right(s) =>
                  }
                  val leftname = be.left.varName
                  handlePendingTask(leftname, PrimitiveType(kind))
                  val newleft = recentvars(leftname)
                  locCode = updateCode(locCode, be.left.id.pos, newleft)
//                  updateEvidence(be.left.id.pos) = newleft
                  rhsType = PrimitiveType(kind)
                case ce: CmpExpression =>
                  val var2name = ce.var2Symbol.varName
                  val typ = ce.paramType
                  handlePendingTask(var2name, typ)
                  val newvar2name = recentvars(var2name)
                  locCode = updateCode(locCode, ce.var2Symbol.id.pos, newvar2name)
//                  updateEvidence(ce.var2Symbol.id.pos) = newvar2name
                  val var1name = ce.var1Symbol.varName
                  handlePendingTask(var1name, typ)
                  val newvar1name = recentvars(var1name)
                  locCode = updateCode(locCode, ce.var1Symbol.id.pos, newvar1name)
//                  updateEvidence(ce.var1Symbol.id.pos) = newvar1name
                  rhsType = PrimitiveType("boolean")
                case ie: InstanceofExpression =>
                  val varname = ie.varSymbol.varName
                  handlePendingTask(varname, JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE)
                  val newarg = recentvars(varname)
                  locCode = updateCode(locCode, ie.varSymbol.id.pos, newarg)
//                  updateEvidence(ie.varSymbol.id.pos) = newarg
                  rhsType = PrimitiveType("boolean")
                case ce: ConstClassExpression =>
                  rhsType = new ObjectType("java.lang.Class")
                case le: LengthExpression =>
                  val varname = le.varSymbol.varName
                  handlePendingTask(varname, JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE)
                  val newarg = recentvars(varname)
                  locCode = updateCode(locCode, le.varSymbol.id.pos, newarg)
//                  updateEvidence(le.varSymbol.id.pos) = newarg
                  rhsType = PrimitiveType("int")
                case ne: NullExpression =>
                  rhsType = JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE
                case _ =>  println("resolveLocalVarType rhs problem: " + as.rhs)
              }
              
              as.lhs match {
                case ne: NameExpression =>
                  ne.varSymbol match {
                    case Left(v) =>
                      if(possibleNull){
                        val code = locCode
                        pendingTasks(ne.name) = ((code, index, v.id.pos, ne.name))
                      } else {
                        var newvar = rhsType.typ.substring(rhsType.typ.lastIndexOf(".") + 1) + {if(rhsType.dimensions > 0)"_arr" + rhsType.dimensions else ""} + "_" + ne.name
                        if(localvars.contains(newvar) && localvars(newvar)._1 != rhsType) newvar = "a" + newvar
                        localvars(newvar) = ((rhsType, false))
                        recentvars(ne.name) = newvar
                        locCode = updateCode(locCode, v.id.pos, newvar)
  //                      updateEvidence(v.id.pos) = newvar
                      }
                    case Right(f) =>
                  }
                case ie: IndexingExpression =>
                  ie.indices.reverse.foreach{
                    indice =>
                      indice.index match {
                        case Left(v) =>
                          val varName = v.varName
                          handlePendingTask(varName, PrimitiveType("int"))
                          val newarg = recentvars(varName)
                          locCode = updateCode(locCode, v.id.pos, newarg)
                        case Right(c) =>
                      }
                  }
                  val varname = ie.base
                  val dimentions = ie.dimentions
                  val typ = JawaType.generateType(rhsType.typ, rhsType.dimensions + dimentions)
                  handlePendingTask(varname, typ)
                  val newarg = recentvars(varname)
                  locCode = updateCode(locCode, ie.varSymbol.id.pos, newarg)
//                  updateEvidence(ie.varSymbol.id.pos) = newarg
                case ae: AccessExpression =>
                  val varname = ae.base
                  handlePendingTask(varname, typOpt.get)
                  val newarg = recentvars(varname)
                  locCode = updateCode(locCode, ae.varSymbol.id.pos, newarg)
//                  updateEvidence(ae.varSymbol.id.pos) = newarg
                case _ => println("resolveLocalVarType lhs problem: " + as.lhs)
              }
            case ts: ThrowStatement => 
              val varname = ts.varSymbol.varName
              handlePendingTask(varname, new ObjectType("java.lang.Throwable"))
              val newarg = recentvars(varname)
              locCode = updateCode(locCode, ts.varSymbol.id.pos, newarg)
//              updateEvidence(ts.varSymbol.id.pos) = newarg
            case is: IfStatement =>
              is.cond.right match {
                case Left(v) =>
                  handlePendingTask(v.varName, PrimitiveType("int"))
                  val newright = recentvars(v.varName)
                  locCode = updateCode(locCode, v.id.pos, newright)
//                  updateEvidence(v.id.pos) = newright
                case Right(c) =>
              }
              val left = is.cond.left.varName
              handlePendingTask(left, PrimitiveType("int"))
              val newleft = recentvars(left)
              locCode = updateCode(locCode, is.cond.left.id.pos, newleft)
//              updateEvidence(is.cond.left.id.pos) = newleft
            case gs: GotoStatement =>
            case ss: SwitchStatement =>
              val varname = ss.condition.varName
              handlePendingTask(varname, PrimitiveType("int"))
              val newvar = recentvars(varname)
              locCode = updateCode(locCode, ss.condition.id.pos, newvar)
//              updateEvidence(ss.condition.id.pos) = newvar
            case rs: ReturnStatement =>
              rs.varOpt match {
                case Some(v) =>
                  val varname = v.varName
                  handlePendingTask(varname, md.signature.getReturnType())
                  val newvar = recentvars(varname)
                  locCode = updateCode(locCode, v.id.pos, newvar)
//                  updateEvidence(v.id.pos) = newvar
                case None =>
              }
            case ms: MonitorStatement =>
              val varname = ms.varSymbol.varName
              handlePendingTask(varname, JavaKnowledge.JAVA_TOPLEVEL_OBJECT_TYPE)
              val newvar = recentvars(varname)
              locCode = updateCode(locCode, ms.varSymbol.id.pos, newvar)
//              updateEvidence(ms.varSymbol.id.pos) = newvar
            case es: EmptyStatement =>
            case _ =>
          }
          if(!possibleNull)
            locations(index) = locCode
        case _ =>
      }
    }
    localvars foreach {
      case (v, (typ, isParam)) =>
        if(!isParam)
          sb.append("      " + typ.jawaName.replace(typ.typ, "`" + typ.typ + "`") + " " + v + ";\n")
    }
    sb.append("\n")
    locations.toList.sortBy(_._1) foreach {
      case (i, loccode) =>
        sb.append(loccode + "\n")
    }
    body.catchClauses foreach {
      cc =>
        sb.append(cc.toCode + "\n")
    }
    sb.append("}")
    sb.toString().trim()
  }
  
  private def updateCode(loccode: String, pos: Position, newtext: String): String = {
    val sb: StringBuffer = new StringBuffer
    sb.append(loccode)
    val start = pos.column
    val end = pos.column + pos.end - pos.start + 1
    sb.replace(start, end, newtext)
    sb.toString().intern()
  }
  
  def resolveCallStatement(code: String): String = {
    val sb: StringBuilder = new StringBuilder
    val reporter = new DefaultReporter
    val cuOpt: Option[CompilationUnit] = JawaParser.parse[CompilationUnit](Left(code), true, reporter)
    cuOpt match {
      case Some(cu) =>
        cu.topDecls.foreach{
          c =>
            val classCode = c.toCode
            val head = if(c.methods.size > 0) code.substring(0, c.methods(0).firstToken.pos.start - c.methods(0).firstToken.pos.column) else code
            sb.append(head)
            c.methods foreach {
              m =>
                val methodcode = RemoveTempAndDoubleLongVars(m)
                sb.append(methodcode + "\n")
            }
        }
      case None =>
        println(reporter.problems)
        throw new RuntimeException()
    }
    sb.toString().trim()
  }
  
  private def RemoveTempAndDoubleLongVars(md: MethodDeclaration): String = {
    val sb: StringBuilder = new StringBuilder
    val body: ResolvedBody = md.body match {
      case rb: ResolvedBody =>
        rb
      case ub: UnresolvedBody =>
        ub.resolve
    }
    val code = md.toCode
    val head: String = code.substring(0, code.indexOf("#") - 1).replaceAll("temp ;", "")
    sb.append(head + "\n")
    var skip: Boolean = false
    body.locations foreach {
      location =>
        var linecode = location.toCode
        if(!skip){
          location.statement match {
            case cs: CallStatement =>
              val typs = cs.signature.getParameterTypes()
              val args = cs.argClause.varSymbols.map(_._1)
              var j = 0
              for(i <- 0 to typs.size - 1) {
                val typ = typs(i)
                typ.name match {
                  case "double" | "long" =>
                    val v1pos = args(j).id.pos
                    val v2pos = args(j+1).id.pos
                    val pos = Position.range(v1pos.source, v1pos.start, v2pos.end - v1pos.start + 1, v1pos.line, v1pos.column)
                    linecode = updateCode(linecode, pos, args(j).varName)
                    j += 1
                  case _ =>
                }
                j += 1
              }
              cs.signature.getReturnType().name match {
                case "void" =>
                  linecode = linecode.replaceAll("temp:=  ", "")
                case _ =>
                  val nextStat = body.locations(location.locationIndex + 1).statement
                  nextStat match {
                    case as: AssignmentStatement =>
                      if(as.rhs.isInstanceOf[NameExpression] && as.rhs.asInstanceOf[NameExpression].name == "temp"){
                        val varName = as.lhs.asInstanceOf[NameExpression].varSymbol.left.get.varName
                        linecode = linecode.replaceFirst("temp:=", varName + ":=")
                        skip = true
                      }
                    case _ =>
                  }
                  
              }
            case _ =>
          }
          sb.append(linecode + "\n")
        } else {
          skip = false
        }
    }
    body.catchClauses foreach {
      cc =>
        sb.append(cc.toCode + "\n")
    }
    sb.append("}")
    sb.toString().trim()
  }
}