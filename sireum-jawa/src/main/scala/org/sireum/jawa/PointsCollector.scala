/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa

import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.util._
import org.sireum.pilar.ast._
import org.sireum.jawa.util.SignatureParser

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
class PointsCollector {
  
  def collectProcPoint(pst : ProcedureSymbolTable) : PointProc = {
    val procSig = 
      pst.procedure.getValueAnnotation("signature") match {
	      case Some(exp : NameExp) =>
	        exp.name.name
	      case _ => throw new RuntimeException("Can not find signature")
	    }
    var retP : Boolean = false
    val parser = new SignatureParser(procSig).getParamSig
    if(parser.isReturnObject){
      retP = true
    } else if(parser.isReturnArray){
      retP = true
    }
    
    val types = parser.getParameters
    
    val access = pst.procedure.getValueAnnotation("Access") match{
      case Some(acc) =>
        acc match {
          case ne : NameExp =>
            ne.name.name
          case _ => ""
        }
      case None => ""
    }

    var i = 0
    pst.procedure.params.foreach(
      param => {
        if(is("this", param.annotations)){
          val thisP = new PointThis(param.name.name, procSig)
          pProc.setThisParam(thisP)
          i-=1
        } else if(is("object", param.annotations)){
          if(types(i).startsWith("[") 
//              && types(i).charAt(types(i).lastIndexOf('[')+1) == 'L'
                ){
            val pPoint = new PointParam(param.name.name, procSig)
            pProc.setParam(i, pPoint)
            val dimensions = types(i).lastIndexOf('[') - types(i).indexOf('[') + 1
//            ofg.arrayRepo(pPoint.toString) = dimensions
          } else if(types(i).startsWith("L")){
            val pPoint = new PointParam(param.name.name, procSig)
            pProc.setParam(i, pPoint)
          }
        }
        i+=1
      }  
    )
    PointProc()
  }
  
  /**
   * Resolve native procedure node collect
   */
  def resolveNativeProc(pst : ProcedureSymbolTable, pProc : PointProc) = {
    val thisP = new PointThis("native", pst.procedureUri)
    pProc.setThisParam(thisP)
  }
  
  /**
     * get type from annotations, if it is an object type return true else false
     */
    def is(typ : String, annots : ISeq[Annotation]) : Boolean = {
      annots.foreach(
        annot => {
          if(annot.name.name.equals(typ)){
            return true
          } else {
            annot.params.foreach(
              param =>{
                if(param.isInstanceOf[ExpAnnotationParam]){
                  param.asInstanceOf[ExpAnnotationParam].exp match {
                    case exp : NameExp =>
                      if(exp.name.name.equals(typ)){
                        return true
                      }
                    case _ => 
                  }
                }
              }
            )
          }
          
        }
      )
      return false
    }
  
  def points(ownerSig : String, pst : ProcedureSymbolTable) : Set[Point] = {
    val points : MSet[Point] = msetEmpty
    var loc : ResourceUri = ""
    var locIndex = 0

    val procPoint = collectProcPoint(pst)
    points += procPoint
    
    def getLocUri(l : LocationDecl) =
        if (l.name.isEmpty)
          ""
        else
          l.name.get.uri
          
    def isGlobal(name : String) : Boolean = {
      if(name.startsWith("@@")){true} else {false}
    }
    
    def initExpPointL(e : Exp) : PointL = {
      e match {
        case n : NameExp =>
          if(isGlobal(n.name.name)){
            new PointGlobalL(n.name.name, loc, locIndex, ownerSig)
          } else {
            new PointL(n.name.name, loc, locIndex, ownerSig)
          }
        case ie : IndexingExp =>
          ie.exp match {
            case n : NameExp =>
              if(isGlobal(n.name.name)){
                new PointGlobalArrayL(n.name.name, loc, locIndex, ownerSig)
              } else {
                val pal = new PointArrayL(n.name.name, loc, locIndex, ownerSig)
                pal.dimensions = ie.indices.size
                pal
              }
            case _ => null
          }
        case _ => null
      }
    }
    
    def initExpPointR(e : Exp) : PointR = {
      e match {
        case n : NameExp =>
          if(isGlobal(n.name.name)){
            new PointGlobalR(n.name.name, loc, locIndex, ownerSig)
          } else {
            new PointR(n.name.name, loc, locIndex, ownerSig)
          }
        case ie : IndexingExp =>
          ie.exp match {
            case n : NameExp =>
              val par = new PointArrayR(n.name.name, loc, locIndex, ownerSig)
              par.dimensions = ie.indices.size
              par
            case _ => null
          }
        case _ => null
      }
    }
    
    def processLHS(lhs : Exp) : PointL = {
      lhs match {
        case n : NameExp => 
          initExpPointL(n)
        case ie : IndexingExp =>
          initExpPointL(ie)
        case a : AccessExp =>
          val baseName = a.exp match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          val pBase = new PointBaseL(baseName, loc, locIndex, ownerSig)
          new PointFieldL(pBase, a.attributeName.name, loc, locIndex, ownerSig)
        case _ => null
      }
    }
      
    val visitor = Visitor.build({
      case ld : LocationDecl => 
        loc = getLocUri(ld)
        locIndex = ld.index
        true
      case t : Transformation =>
        true
      case as : AssignAction =>
        var pl : PointL = null
        var pr : PointR = null
        as.rhs match {
          case le : LiteralExp =>
            if(le.typ.name.equals("STRING")){
              pl = processLHS(as.lhs)
              pr = new PointStringO(le.text, "java.lang.String", loc, locIndex, ownerSig)
//              ofg.iFieldDefRepo(pr.asInstanceOf[PointStringO]) = mmapEmpty
            }
          case n : NewExp =>
            pl = processLHS(as.lhs)
            var name : ResourceUri = ""
            var dimensions = 0
            n.typeSpec match {
              case nt : NamedTypeSpec => 
                dimensions = n.dims.size + n.typeFragments.size
                name = nt.name.name
              case _ =>
            }
            if(dimensions == 0){
              pr = new PointO(name, loc, locIndex, ownerSig)
//              ofg.iFieldDefRepo(pr.asInstanceOf[PointO]) = mmapEmpty
            } else {
              pr = new PointArrayO(name, loc, locIndex, ownerSig)
              pr.asInstanceOf[PointArrayO].dimensions = dimensions
//              ofg.arrayRepo(pl.toString) = dimensions
            }
          case n : NameExp =>
            if(is("object", as.annotations)){
              pl = processLHS(as.lhs)
              if(is("array", as.annotations) && isGlobal(n.name.name)){
                pr = new PointGlobalArrayR(n.name.name, loc, locIndex, ownerSig)
              } else {
                pr = initExpPointR(n)
              }
            }
          case ae : AccessExp =>
            if(is("object", as.annotations)){
              pl = processLHS(as.lhs)
              val baseName = ae.exp match {
                case ne : NameExp => ne.name.name
                case _ => ""
              }
              val pBase = new PointBaseR(baseName, loc, locIndex, ownerSig)
              pr = new PointFieldR(pBase, ae.attributeName.name, loc, locIndex, ownerSig)
            }
          case ie : IndexingExp =>
            if(is("object", as.annotations)){
              pl = processLHS(as.lhs)
              pr = initExpPointR(ie)
            }
          case _ =>
        }
        if(pl != null && pr != null){
          val assignmentPoint : PointAsmt = new PointAsmt("[" + pl + "=" + pr + "]", loc, locIndex, ownerSig)
          assignmentPoint.lhs = pl
          assignmentPoint.rhs = pr
          points += assignmentPoint
        }
        false
      case t : CallJump if t.jump.isEmpty =>
        var pl : PointL = null
        val sig = t.getValueAnnotation("signature") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => ""
        }
        val typ = t.getValueAnnotation("type") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => ""
        }
        val types = new SignatureParser(sig).getParamSig.getParameters
        val pi : PointI = new PointI(sig, loc, locIndex, ownerSig)
        pi.typ = typ
        pi.retTyp = new SignatureParser(sig).getParamSig.getReturnTypeSignature
        
        t.callExp.arg match {
          case te : TupleExp =>{
            val exps = te.exps
            if(pi.typ.equals("static")){
              for(i <- 0 to (exps.size-1)) {
                exps(i) match {
                  case ne : NameExp =>
                    if(types.size > i){
                      val typ = types(i)
                      if(typ.startsWith("L")){
                        val arg = new PointArg(ne.name.name, loc, locIndex, ownerSig)
                        arg.container = pi
                        pi.setArg(i, arg)
                      } else if(typ.startsWith("[") 
  //                        && typ.charAt(pi.retTyp.lastIndexOf('[')+1) == 'L'
                            ){
                        val arg = new PointArg(ne.name.name, loc, locIndex, ownerSig)
                        arg.container = pi
                        pi.setArg(i, arg)
                        val dimensions = typ.lastIndexOf('[') - typ.indexOf('[') + 1
//                        ofg.arrayRepo(pi.args_Call(i).toString) = dimensions
                      }
                    }
                  case _ =>
                }
              }
            } else {
              if(exps.size > 0){
                exps(0) match {
                  case ne : NameExp =>
                    val recv = new PointRecv(ne.name.name, loc, locIndex, ownerSig)
                    recv.container = pi
                    pi.setRecv(recv)
                  case _ =>
                }
                for(i <- 1 to (exps.size-1)) {
                  exps(i) match {
                    case ne : NameExp =>
                      if(types.size > i-1){
                        val typ = types(i-1)
                        if(typ.startsWith("L")){
                          val arg = new PointArg(ne.name.name, loc, locIndex, ownerSig)
                          arg.container = pi
                          pi.setArg(i-1, arg)
                        } else if(typ.startsWith("[") 
  //                          && typ.charAt(pi.retTyp.lastIndexOf('[')+1) == 'L'
                              ){
                          val arg = new PointArg(ne.name.name, loc, locIndex, ownerSig)
                          arg.container = pi
                          pi.setArg(i-1, arg)
                          val dimensions = typ.lastIndexOf('[') - typ.indexOf('[') + 1
//                          ofg.arrayRepo(pi.args_Call(i-1).toString) = dimensions                        
                        }
                      }
                    case _ =>
                  }
                }
              }
            }
          }
          case _ =>
        }
        points += pi
        if(pi.retTyp.startsWith("L")){
          require(t.lhss.size<=1)
          if(t.lhss.size == 1){
           pl = new PointL(t.lhss(0).name.name, loc, locIndex, ownerSig)
          }
          //Note that we are considering "call temp = invoke" as an assignment
          val assignmentPoint : PointAsmt = new PointAsmt("[" + pl + "=" + pi + "]", loc, locIndex, ownerSig)
          assignmentPoint.lhs = pl
          assignmentPoint.rhs = pi
          points += assignmentPoint
        } else if(pi.retTyp.startsWith("[")){
//          if(pi.retTyp.charAt(pi.retTyp.lastIndexOf('[')+1) == 'L'){
          require(t.lhss.size<=1)
          if(t.lhss.size == 1){
           pl = new PointL(t.lhss(0).name.name, loc, locIndex, ownerSig)
          }
          val dimensions = pi.retTyp.lastIndexOf('[') - pi.retTyp.indexOf('[') + 1
//            ofg.arrayRepo(pl.toString) = dimensions
          val assignmentPoint : PointAsmt = new PointAsmt("[" + pl + "=" + pi + "]", loc, locIndex, ownerSig)
          assignmentPoint.lhs = pl
          assignmentPoint.rhs = pi
          points += assignmentPoint
//          }
        }
        false
      case rj : ReturnJump =>
        if(is("object", rj.annotations)){
          rj.exp match {
            case Some(ne) =>
              if(ne.isInstanceOf[NameExp]){
                val p = new PointRet(ne.asInstanceOf[NameExp].name.name, loc, locIndex, ownerSig)
                p.procPoint = procPoint
                points += p
              }
            case None =>
          }
        }
        false
    }) 
    
    val locationDecls = pst.locations.toSeq
    val size = locationDecls.size
    for (i <- 0 until size) {
      val l = locationDecls(i)
      visitor(l)
    }
//    println("points---> " + points)
    points.toSet
  }
}

/**
 * Set of program points corresponding to l-value expressions. 
 * pl represents an element in this set.
 */
trait Left

/**
 * Set of program points corresponding to r-value expressions. 
 * This also include expressions which evaluate to void. 
 * pr represents an element in this set. Pr=P\Pl
 */
trait Right

/**
 * global variable
 */
trait Global{def globalsig : String}

/**
 * array
 */
trait Array{def dimensions : Int}

/**
 * object creation
 */
trait NewObj{def obj : String}

/**
 * base variable
 */
trait Base{def basename : String}

/**
 * field variable
 */
trait Field{
  def base : Base
  def fieldSig : String
}

trait Point{def ownersig : String}

/**
 * have location and index
 */
trait Loc{
  def loc : ResourceUri
  def locIndex : Int
}

/**
 * entry and exit point which has identifier
 */
trait Identifier{def identifier : String}

trait Virtual{def recv : PointRecv}

trait Invoke{def args : ISet[PointArg]}

trait Proc{
  def procSig : String
  def accessTyp : String
  def params : ISet[PointParam]
  def ret : Boolean
}

/**
 * Set of program points corresponding to assignment expression. 
 */
final case class PointAsmt(lhs : Point with Left, rhs : Point with Right, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc

/**
 * Set of program points corresponding to method recv variable.
 * pi represents an element in this set.
 */
final case class PointRecv(recvname : ResourceUri, container : PointI, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Right

/**
 * Set of program points corresponding to method arg variable.
 * pi represents an element in this set.
 */
final case class PointArg(argname : String, index : Int, container : PointI, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Right

/**
 * Set of program points corresponding to method invocation expressions.
 * pi represents an element in this set.
 */
final case class PointI(sig : String, retTyp : String, recv : PointRecv, args : ISet[PointArg], loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Right with Invoke with Virtual

/**
 * Set of program points corresponding to static method invocation expressions.
 * pi represents an element in this set.
 */
final case class PointStaticI(sig : String, retTyp : String, args : ISet[PointArg], loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Right with Invoke

/**
 * Set of program points corresponding to object creating expressions. 
 * An object creating program point abstracts all the objects created
 * at that particular program point.
 */
final case class PointO(obj : String, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Right with NewObj

/**
 * Set of program points corresponding to array object creating expressions. 
 * An array object creating program point abstracts all the objects created
 * at that particular program point.
 */
final case class PointArrayO(obj : String, dimensions : Int, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Right with NewObj with Array

/**
 * Set of program points corresponding to string object creating expressions. 
 * An string object creating program point abstracts all the objects created
 * at that particular program point.
 */
final case class PointStringO(obj : String, text : String, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Right with NewObj

/**
 * Set of program points corresponding to l-value field access expressions. 
 */
final case class PointFieldL(base : Base with Left, fieldSig : String, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Left with Field

/**
 * Set of program points corresponding to R-value field access expressions. 
 */
final case class PointFieldR(base : Base with Right, fieldSig : String, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Right with Field

/**
 * Set of program points corresponding to l-value array variable. 
 */
final case class PointArrayL(arrayname : String, dimensions: Int, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Left with Array

/**
 * Set of program points corresponding to R-value array variable. 
 */
final case class PointArrayR(arrayname : String, dimensions: Int, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Right with Array

/**
 * Set of program points corresponding to l-value global variable. 
 */
final case class PointGlobalL(globalsig : String, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Left with Global

/**
 * Set of program points corresponding to R-value global variable. 
 */
final case class PointGlobalR(globalsig : String, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Right with Global

/**
 * Set of program points corresponding to l-value global array variable. 
 */
final case class PointGlobalArrayL(globalsig : String, dimensions: Int, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Left with Global with Array

/**
 * Set of program points corresponding to R-value global array variable. 
 */
final case class PointGlobalArrayR(globalsig : String, dimensions: Int, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Right with Global with Array

/**
 * Set of program points corresponding to base part of field access expressions in the LHS. 
 */
final case class PointBaseL(basename : String, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Left with Base

/**
 * Set of program points corresponding to base part of field access expressions in the RHS. 
 */
final case class PointBaseR(basename : String, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc with Right with Base

/**
 * Set of program points corresponding to this variable .
 */
final case class PointThis(thisname : String, identifier : String, ownersig : String) extends Point with Identifier

/**
 * Set of program points corresponding to params.
 */
final case class PointParam(paramname : String, index : Int, identifier : String, ownersig : String) extends Point with Identifier

/**
 * Set of program points corresponding to return variable.
 */
final case class PointRet(retname : String, loc : ResourceUri, locIndex : Int, ownersig : String) extends Point with Loc

/**
 * Set of program points corresponding to procedures. 
 */
final case class PointProc(procSig : String, ret : Boolean, accessTyp : String, thisParam_Entry : PointThis, params : ISet[PointParam], ownersig : String) extends Point with Proc

/**
 * Set of program points corresponding to static procedures. 
 */
final case class PointProcStatic(procSig : String, ret : Boolean, accessTyp : String, params : ISet[PointParam], ownersig : String) extends Point with Proc