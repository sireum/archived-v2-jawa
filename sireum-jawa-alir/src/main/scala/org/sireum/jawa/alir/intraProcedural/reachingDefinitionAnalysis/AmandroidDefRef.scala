package org.sireum.jawa.alir.intraProcedural.reachingDefinitionAnalysis

import org.sireum.pilar.symbol.SymbolTable
import org.sireum.alir.DefRef
import org.sireum.alir.VarAccesses
import org.sireum.pilar.ast._
import org.sireum.util._
import org.sireum.alir.Slot
import org.sireum.pilar.symbol.H
import org.sireum.alir.VarSlot
import org.sireum.pilar.symbol.Symbol.pp2r
import org.sireum.jawa.JawaCodeSource
import org.sireum.jawa.Transform
import org.sireum.jawa.Center
import org.sireum.jawa.JawaResolver
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.MessageCenter._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
final class AmandroidVarAccesses(st: SymbolTable) extends VarAccesses {
  def localVarAccesses(procedureUri: ResourceUri): CSet[Slot] =
    procedureLocalAccessCache(procedureUri)

  def globalVarReads(procedureUri: ResourceUri): CSet[Slot] =
    procedureGlobalReadCache(procedureUri)

  def globalVarWrites(procedureUri: ResourceUri): CSet[Slot] =
    procedureGlobalWriteCache(procedureUri)

  def strongGlobalVarWrites(procedureUri: ResourceUri): CSet[Slot] =
    Set()

  private val (procedureLocalAccessCache, procedureGlobalReadCache, procedureGlobalWriteCache) = {
    val localAccesses = mmapEmpty[ResourceUri, MSet[Slot]]
    val globalReads = mmapEmpty[ResourceUri, MSet[Slot]]
    val globalWrites = mmapEmpty[ResourceUri, MSet[Slot]]

    def init() {
      var accessLocalVars = msetEmpty[Slot]
      var readGlobalVars = msetEmpty[Slot]
      var writtenGlobalVars = msetEmpty[Slot]

      def addLocalAccess(ne: NameExp) =
        if (H.isLocalVar(ne.name))
          accessLocalVars += VarSlot(ne.name.uri)

      val visitor = Visitor.build({
        case a: Assignment =>
          val lhss = PilarAstUtil.getLHSs(a)
          for (NameExp(name) <- lhss.keys)
            if (name.hasResourceInfo && H.isGlobalVar(name))
              writtenGlobalVars += VarSlot(name.uri)
          Visitor.build({
            case ne: NameExp =>
              if (ne.name.hasResourceInfo)
                if (H.isGlobalVar(ne.name) && !lhss.contains(ne))
                  readGlobalVars += VarSlot(ne.name.uri)
                else
                  addLocalAccess(ne)
              false
          })(a)
          false
        case ne: NameExp =>
          if (ne.name.hasResourceInfo)
            if (H.isGlobalVar(ne.name))
              readGlobalVars += VarSlot(ne.name.uri)
            else
              addLocalAccess(ne)
          false
      })

      st.procedureSymbolTables.map { pst =>
        val p = pst.procedure
        p.params.foreach{
          case param=>
            if (H.isLocalVar(param.name))
            	accessLocalVars += VarSlot(param.name.uri)
        }
        visitor(p)
        localAccesses(pst.procedureUri) = accessLocalVars
        globalReads(pst.procedureUri) = readGlobalVars
        globalWrites(pst.procedureUri) = writtenGlobalVars
        accessLocalVars = msetEmpty[Slot]
        readGlobalVars = msetEmpty[Slot]
        writtenGlobalVars = msetEmpty[Slot]
      }
    }
    init()
    (localAccesses, globalReads, globalWrites)
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
final class AmandroidDefRef(st: SymbolTable, val varAccesses: VarAccesses)
  extends DefRef {

  final val TITLE = "AmandroidDefRef"
  
  def definitions(a: Assignment): ISet[Slot] = {
    import org.sireum.pilar.symbol.H

    strongDefinitions(a)
  }
  
  private def is(typ : String, annots : ISeq[Annotation]) : Boolean = {
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

  def strongDefinitions(a: Assignment): ISet[Slot] =
    defCache.getOrElseUpdate(a, {
      val lhss = PilarAstUtil.getLHSs(a)
      var result = isetEmpty[Slot]
      def resolveNameExp(ne : NameExp) = {
        var uri : ResourceUri = null
        if(!ne.name.hasResourceInfo){
          val recType = StringFormConverter.getRecordTypeFromFieldSignature(ne.name.name)
          val gf = Center.findField(recType, ne.name.name).getOrElse(throw new RuntimeException("cannot find field: " + ne.name.name))
          Center.getGlobalVarUri(gf.getSignature) match{
            case Some(u) => uri = u
            case None =>
          }
          if(uri == null) err_msg_critical(TITLE, "global var " + ne.name.name + " cannot resolved.")
        } else {
          uri = ne.name.uri
        }
        if(uri != null) result += VarSlot(uri)
      }
      lhss.keys.foreach{
        case key=>
          key match{
            case ne : NameExp =>
              resolveNameExp(ne)
            case _=>
          }
      }
      result
    })

  def references(a: Action): ISet[Slot] =
    refCache.getOrElseUpdate(a, getRefs(a))

  def references(j: Jump): ISet[Slot] =
    refCache.getOrElseUpdate(j, getRefs(j))

  def callReferences(j: CallJump): ISeq[ISet[Slot]] = {
//    val arg = j.callExp.arg
//    arg match {
//      case e: TupleExp =>
//        val result = e.exps.map { exp => refCache.getOrElseUpdate(exp, getRefs(exp)) }
//        result
//      case e =>
//        ivector(refCache.getOrElseUpdate(j, getRefs(e)))
//    }
    ivectorEmpty
  }

  def callDefinitions(j: CallJump): ISeq[ISet[Slot]] = {
    callReferences(j)
  }

  private def getRefs(n: PilarAstNode): ISet[Slot] = {
    var result = isetEmpty[Slot]
    val lhss = PilarAstUtil.getLHSs(n)
    Visitor.build({
      case ne: NameExp =>
        if (!lhss.contains(ne) && ne.name.hasResourceInfo)
          result = result + VarSlot(ne.name.uri)
        false
    })(n)
    result
  }

  private val defCache = idmapEmpty[Assignment, ISet[Slot]]
  private val refCache = idmapEmpty[PilarAstNode, ISet[Slot]]
}