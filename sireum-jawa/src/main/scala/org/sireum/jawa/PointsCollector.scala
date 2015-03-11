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
import org.sireum.jawa.util.StringFormConverter

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
class PointsCollector {
  
  def collectProcPoint(ownerSig : String, pst : ProcedureSymbolTable) : Point with Proc = {
    val procSig = 
      pst.procedure.getValueAnnotation("signature") match {
	      case Some(exp : NameExp) =>
	        exp.name.name
	      case _ => throw new RuntimeException("Can not find signature")
	    }
    
    val parser = new SignatureParser(procSig).getParamSig
    val types = parser.getParameterTypes()
    val thisTyp = StringFormConverter.getRecordTypeFromProcedureSignature(procSig)
    
    val accessTyp = pst.procedure.getValueAnnotation("Access") match{
      case Some(acc) =>
        acc match {
          case ne : NameExp =>
            ne.name.name
          case _ => ""
        }
      case None => ""
    }

    var thisPEntry : PointThisEntry = null
    var thisPExit : PointThisExit = null
    val paramPsEntry : MMap[Int, PointParamEntry] = mmapEmpty
    val paramPsExit : MMap[Int, PointParamExit] = mmapEmpty
    var i = 0 // control param index to be the exact position (including this) 
    var j = 0 // control type traversal
    pst.procedure.params.foreach(
      param => {
        if(is("this", param.annotations)){
          thisPEntry = PointThisEntry(param.name.name, thisTyp, i, ownerSig)
          thisPExit = PointThisExit(param.name.name, thisTyp, i, ownerSig)
          j -= 1
        } else if(is("object", param.annotations)){
          paramPsEntry += (i -> PointParamEntry(param.name.name, types(j), i, ownerSig))
          paramPsExit += (i -> PointParamExit(param.name.name, types(j), i, ownerSig))
        }
        i += 1
        j += 1
      }
    )
    
    var retP : Option[PointProcRet] = None
    if(parser.isReturnObject || parser.isReturnArray){
      retP = Some(PointProcRet(procSig))
    }
    
    if(AccessFlag.isStatic(AccessFlag.getAccessFlags(accessTyp))){
      PointStaticProc(procSig, accessTyp, paramPsEntry.toMap, paramPsExit.toMap, retP, ownerSig)
    } else {
      if(thisPEntry == null) throw new RuntimeException("Virtual method does not have 'this' param.")
      PointProc(procSig, accessTyp, thisPEntry, thisPExit, paramPsEntry.toMap, paramPsExit.toMap, retP, ownerSig)
    }
  }
  
  /**
   * Resolve native procedure node collect
   */
//  def resolveNativeProc(pst : ProcedureSymbolTable, pProc : PointProc) = {
//    val thisP = PointThis("native", pst.procedureUri)
//    pProc.setThisParam(thisP)
//  }
  
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

    val procPoint = collectProcPoint(ownerSig, pst)
    points += procPoint
    
    def getLocUri(l : LocationDecl) =
        if (l.name.isEmpty)
          ""
        else
          l.name.get.uri
          
    def isGlobal(name : String) : Boolean = {
      if(name.startsWith("@@")){true} else {false}
    }
    
    def initExpPointL(e : Exp) : Point with Left = {
      e match {
        case n : NameExp =>
          if(isGlobal(n.name.name)){
            PointGlobalL(n.name.name, loc, locIndex, ownerSig)
          } else {
            PointL(n.name.name, loc, locIndex, ownerSig)
          }
        case ie : IndexingExp =>
          ie.exp match {
            case n : NameExp =>
              val dimensions = ie.indices.size
              if(isGlobal(n.name.name)){
                PointGlobalArrayL(n.name.name, dimensions, loc, locIndex, ownerSig)
              } else {
                PointArrayL(n.name.name, dimensions, loc, locIndex, ownerSig)
              }
            case _ => null
          }
        case _ => null
      }
    }
    
    def initExpPointR(e : Exp) : Point with Right = {
      e match {
        case n : NameExp =>
          if(isGlobal(n.name.name)){
            PointGlobalR(n.name.name, loc, locIndex, ownerSig)
          } else {
            PointR(n.name.name, loc, locIndex, ownerSig)
          }
        case ie : IndexingExp =>
          val dimensions = ie.indices.size
          ie.exp match {
            case n : NameExp =>
              if(isGlobal(n.name.name)){
                PointGlobalArrayR(n.name.name, dimensions, loc, locIndex, ownerSig)
              } else {
                PointArrayR(n.name.name, dimensions, loc, locIndex, ownerSig)
              }
            case _ => null
          }
        case _ => null
      }
    }
    
    def processLHS(lhs : Exp) : Point with Left = {
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
          val pBase = PointBaseL(baseName, loc, locIndex, ownerSig)
          val fSig = a.attributeName.name
          val fName = StringFormConverter.getFieldNameFromFieldSignature(fSig)
          val pfl = PointFieldL(pBase, fName, loc, locIndex, ownerSig)
          pBase.setFieldPoint(pfl)
          pfl
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
        var pl : Point with Left = null
        var pr : Point with Right = null
        as.rhs match {
          case le : LiteralExp =>
            if(le.typ.name.equals("STRING")){
              pl = processLHS(as.lhs)
              pr = PointStringO(le.text, "java.lang.String", loc, locIndex, ownerSig)
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
              pr = PointO(name, loc, locIndex, ownerSig)
            } else {
              pr = PointArrayO(name, dimensions, loc, locIndex, ownerSig)
            }
          case n : NameExp =>
            if(is("object", as.annotations)){
              pl = processLHS(as.lhs)
              pr = initExpPointR(n)
            }
          case ae : AccessExp =>
            if(is("object", as.annotations)){
              pl = processLHS(as.lhs)
              val baseName = ae.exp match {
                case ne : NameExp => ne.name.name
                case _ => ""
              }
              val pBase = PointBaseR(baseName, loc, locIndex, ownerSig)
              val fSig = ae.attributeName.name
              val fName = StringFormConverter.getFieldNameFromFieldSignature(fSig)
              val pfr = PointFieldR(pBase, fName, loc, locIndex, ownerSig)
              pBase.setFieldPoint(pfr)
              pr = pfr
            }
          case ie : IndexingExp =>
            pl = processLHS(as.lhs)
            pr = initExpPointR(ie)
          case _ =>
        }
        if(pl != null && pr != null){
          val assignmentPoint : PointAsmt = PointAsmt(pl, pr, loc, locIndex, ownerSig)
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
        val invokeTyp = t.getValueAnnotation("type") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => ""
        }

        val parser = new SignatureParser(sig).getParamSig
        val retTyp = parser.getReturnType
        val paramTyps = parser.getParameterTypes
        
        var recvPCall : PointRecvCall = null
        var recvPReturn : PointRecvReturn = null
        val argPsCall : MMap[Int, PointArgCall] = mmapEmpty
        val argPsReturn : MMap[Int, PointArgReturn] = mmapEmpty
        var i = 0
        var j = 0
        var ignore = false
        t.callExp.arg match {
          case te : TupleExp =>{
            val exps = te.exps
            exps foreach {
              exp =>
                if(!ignore){
                  require(exp.isInstanceOf[NameExp])
                  val ne = exp.asInstanceOf[NameExp]
                  if(i == 0 && !invokeTyp.contains("static")){
                    recvPCall = PointRecvCall(ne.name.name, i, loc, locIndex, ownerSig)
                    recvPReturn = PointRecvReturn(ne.name.name, i, loc, locIndex, ownerSig)
                    j -= 1
                  } else {
                    argPsCall += (i -> PointArgCall(ne.name.name, i, loc, locIndex, ownerSig))
                    argPsReturn += (i -> PointArgReturn(ne.name.name, i, loc, locIndex, ownerSig))
                    if(paramTyps(j).name == "long" || paramTyps(j).name == "double"){
                      ignore = true
                    }
                  }
                  i += 1
                  j += 1
                } else ignore = false
            }
          }
          case _ =>
        }
        val pi : Point with Right with Invoke = 
          if(invokeTyp.contains("static")){
            PointStaticI(sig, invokeTyp, retTyp, argPsCall.toMap, argPsReturn.toMap, loc, locIndex, ownerSig)
          } else {
            if(recvPCall == null) throw new RuntimeException("Dynamic method invokation does not have 'recv' param.")
            val p = PointI(sig, invokeTyp, retTyp, recvPCall, recvPReturn, argPsCall.toMap, argPsReturn.toMap, loc, locIndex, ownerSig)
            recvPCall.setContainer(p)
            recvPReturn.setContainer(p)
            p
          }
        argPsCall foreach {case (_, p) => p.setContainer(pi)}
        argPsReturn foreach {case (_, p) => p.setContainer(pi)}
        points += pi
        require(t.lhss.size<=1)
        if(t.lhss.size == 1){
          pl = PointL(t.lhss(0).name.name, loc, locIndex, ownerSig)
        }
        //Note that we are considering "call temp = invoke" as an assignment
        val assignmentPoint : PointAsmt = PointAsmt(pl, pi, loc, locIndex, ownerSig)
        points += assignmentPoint
        false
      case rj : ReturnJump =>
        if(is("object", rj.annotations)){
          rj.exp match {
            case Some(ne) =>
              if(ne.isInstanceOf[NameExp]){
                val p = PointRet(ne.asInstanceOf[NameExp].name.name, procPoint, loc, locIndex, ownerSig)
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
 * Set of program points corresponding to assignment expression. 
 */
final case class PointAsmt(lhs : Point with Left, rhs : Point with Right, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc

/**
 * Set of program points corresponding to object creating expressions. 
 * An object creating program point abstracts all the objects created
 * at that particular program point.
 */
final case class PointO(obj : String, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Right with NewObj

/**
 * Set of program points corresponding to array object creating expressions. 
 * An array object creating program point abstracts all the objects created
 * at that particular program point.
 */
final case class PointArrayO(obj : String, dimensions : Int, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Right with NewObj with Array

/**
 * Set of program points corresponding to string object creating expressions. 
 * An string object creating program point abstracts all the objects created
 * at that particular program point.
 */
final case class PointStringO(obj : String, text : String, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Right with NewObj

/**
 * Set of program points corresponding to l-value. 
 */
final case class PointL(varname : String, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Left

/**
 * Set of program points corresponding to r-value. 
 */
final case class PointR(varname : String, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Right

/**
 * Set of program points corresponding to l-value field access expressions. 
 */
final case class PointFieldL(baseP : PointBaseL, fieldName : String, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Left with Field

/**
 * Set of program points corresponding to R-value field access expressions. 
 */
final case class PointFieldR(baseP : PointBaseR, fieldName : String, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Right with Field

/**
 * Set of program points corresponding to l-value array variable. 
 */
final case class PointArrayL(arrayname : String, dimensions: Int, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Left with Array

/**
 * Set of program points corresponding to R-value array variable. 
 */
final case class PointArrayR(arrayname : String, dimensions: Int, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Right with Array

/**
 * Set of program points corresponding to l-value global variable. 
 */
final case class PointGlobalL(globalSig : String, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Left with Global

/**
 * Set of program points corresponding to R-value global variable. 
 */
final case class PointGlobalR(globalSig : String, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Right with Global

/**
 * Set of program points corresponding to l-value global array variable. 
 */
final case class PointGlobalArrayL(globalSig : String, dimensions: Int, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Left with Global with Array

/**
 * Set of program points corresponding to R-value global array variable. 
 */
final case class PointGlobalArrayR(globalSig : String, dimensions: Int, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Right with Global with Array

/**
 * Set of program points corresponding to base part of field access expressions in the LHS. 
 */
final case class PointBaseL(baseName : String, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Left with Base

/**
 * Set of program points corresponding to base part of field access expressions in the RHS. 
 */
final case class PointBaseR(baseName : String, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Right with Base

/**
 * Set of program points corresponding to method recv variable.
 * pi represents an element in this set.
 */
final case class PointRecvCall(argName : String, index : Int, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Right with Arg with Call

/**
 * Set of program points corresponding to method arg variable.
 * pi represents an element in this set.
 */
final case class PointArgCall(argName : String, index : Int, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Right with Arg with Call

/**
 * Set of program points corresponding to method recv variable.
 * pi represents an element in this set.
 */
final case class PointRecvReturn(argName : String, index : Int, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Right with Arg with Return

/**
 * Set of program points corresponding to method arg variable.
 * pi represents an element in this set.
 */
final case class PointArgReturn(argName : String, index : Int, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Right with Arg with Return

/**
 * Set of program points corresponding to method invocation expressions.
 * pi represents an element in this set.
 */
final case class PointI(sig : String, invokeTyp : String, retTyp : Type, recvPCall : PointRecvCall, recvPReturn : PointRecvReturn, argPsCall : IMap[Int, PointArgCall], argPsReturn : IMap[Int, PointArgReturn], loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Right with Invoke with Dynamic

/**
 * Set of program points corresponding to static method invocation expressions.
 * pi represents an element in this set.
 */
final case class PointStaticI(sig : String, invokeTyp : String, retTyp : Type, argPsCall : IMap[Int, PointArgCall], argPsReturn : IMap[Int, PointArgReturn], loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc with Right with Invoke

/**
 * Set of program points corresponding to this variable .
 */
final case class PointThisEntry(paramName : String, paramTyp : Type, index : Int, ownerSig : String) extends Point with Param with Entry

/**
 * Set of program points corresponding to params.
 */
final case class PointParamEntry(paramName : String, paramTyp : Type, index : Int, ownerSig : String) extends Point with Param with Entry

/**
 * Set of program points corresponding to this variable .
 */
final case class PointThisExit(paramName : String, paramTyp : Type, index : Int, ownerSig : String) extends Point with Param with Exit

/**
 * Set of program points corresponding to params.
 */
final case class PointParamExit(paramName : String, paramTyp : Type, index : Int, ownerSig : String) extends Point with Param with Exit

/**
 * Set of program points corresponding to return variable.
 */
final case class PointRet(retname : String, procPoint : Point with Proc, loc : ResourceUri, locIndex : Int, ownerSig : String) extends Point with Loc

/**
 * Set of program points corresponding to return variable (fake one).
 */
final case class PointProcRet(ownerSig : String) extends Point

/**
 * Set of program points corresponding to procedures. 
 */
final case class PointProc(procSig : String, accessTyp : String, thisPEntry : PointThisEntry, thisPExit : PointThisExit, paramPsEntry : IMap[Int, PointParamEntry], paramPsExit : IMap[Int, PointParamExit], retVar : Option[PointProcRet], ownerSig : String) extends Point with Proc with Virtual

/**
 * Set of program points corresponding to static procedures. 
 */
final case class PointStaticProc(procSig : String, accessTyp : String, paramPsEntry : IMap[Int, PointParamEntry], paramPsExit : IMap[Int, PointParamExit], retVar : Option[PointProcRet], ownerSig : String) extends Point with Proc 