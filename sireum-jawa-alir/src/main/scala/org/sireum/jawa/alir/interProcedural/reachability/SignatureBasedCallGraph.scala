package org.sireum.jawa.alir.interProcedural.reachability

import org.sireum.jawa.JawaProcedure
import org.sireum.util._
import org.sireum.jawa.Center
import org.sireum.pilar.ast.LocationDecl
import org.sireum.pilar.ast.CallJump
import org.sireum.pilar.ast.NameExp
import org.sireum.jawa.JawaRecord

object SignatureBasedCallGraph {
  def getReachableProcedures(record : JawaRecord, wholeProcs : Set[JawaProcedure], par : Boolean) : Set[JawaProcedure] = {
    require(!record.isInterface && !record.isPhantom)
    getReachableProcedures(record.getProcedures, wholeProcs, par)
  }
  
	def getReachableProcedures(procedures : Set[JawaProcedure], appProcs : Set[JawaProcedure], par : Boolean) : Set[JawaProcedure] = {
	  var result : Set[JawaProcedure] = Set()
	  val processed = mlistEmpty[JawaProcedure]
	  var workList = isetEmpty[JawaProcedure]
    workList ++= procedures
    while(!workList.isEmpty){
      val tmp =
			  (if (par) workList else workList).map{
			    proc =>
			      if(proc.isConcrete){
			        if(!proc.hasProcedureBody) proc.resolveBody
			        val body = proc.getProcedureBody
			        body.locations.map{
			          loc =>
			            visitLoc(loc, appProcs)
			        }.reduce(iunion[JawaProcedure])
			      } else Set[JawaProcedure]()
			  }.reduce(iunion[JawaProcedure])
			processed ++= workList
			workList = tmp.filter(!processed.contains(_))
    }
	  result
  }
	
	
	def visitLoc(loc : LocationDecl, appProcs : Set[JawaProcedure]) : Set[JawaProcedure] = {
	  var result = Set[JawaProcedure]()
	  val visitor = Visitor.build({
      case t : CallJump if t.jump.isEmpty =>
        val sig = t.getValueAnnotation("signature") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => ""
        }
        val recName = Center.getRecordNameFromProcedureSignature(sig)
        val subSig = Center.getSubSigFromProcSig(sig)
        val flag : Boolean = appProcs.exists(p => p.getSubSignature == subSig)
        val rec = Center.resolveRecord(recName, Center.ResolveLevel.HIERARCHY)
        val typ = t.getValueAnnotation("type") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => ""
        }
        typ match{
          case "virtual" | "interface" | "super" =>
            if(flag){
	            if(rec.isInterface){
	               Center.getRecordHierarchy.getAllImplementersOf(rec).foreach{
	                 record =>
	                  if(record.isApplicationRecord && record.isConcrete && record.declaresProcedure(subSig)) result += record.getProcedure(subSig)
	               }
	            } else {
	              Center.getRecordHierarchy.getAllSubClassesOfIncluding(rec).foreach{
	                record =>
	                  if(record.isApplicationRecord && record.isConcrete && record.declaresProcedure(subSig)) result += record.getProcedure(subSig)
	              }
	            }
            }
          case "direct" | "static" =>
            if(flag){
            	if(rec.isApplicationRecord && rec.isConcrete && rec.declaresProcedure(subSig)) result += rec.getProcedure(subSig)
            }
        }
        false
    })
    
    visitor(loc)
    result
	}
}