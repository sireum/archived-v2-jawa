package org.sireum.jawa.alir.interProcedural.reachability

import org.sireum.jawa.JawaProcedure
import scala.collection.GenSet
import org.sireum.util._
import org.sireum.jawa.Center
import org.sireum.pilar.ast.LocationDecl
import org.sireum.pilar.ast.CallJump
import org.sireum.pilar.ast.NameExp

object BackwardCallChain {
  
  def getReachableProceduresBySubSig(apiSubSig : String, par : Boolean) : Set[JawaProcedure] = {
    var result : ISet[JawaProcedure] = isetEmpty
	  val ps : Set[JawaProcedure] = Center.getApplicationRecords.map(_.getProcedures).reduce(iunion[JawaProcedure])
	  val workList = mlistEmpty[String]
    val processed = msetEmpty[String]
	  result ++=
		  (if (par) ps.par else ps).filter{
		    proc =>
		      var flag : Boolean = false
		      if(proc.isConcrete){
		        if(!proc.hasProcedureBody) proc.resolveBody
		        val body = proc.getProcedureBody
		        flag = body.locations.map{
		          loc =>
		            visitLocUseSubSig(apiSubSig, loc)
		        }.exists(_ == true)
		      }
		      if(flag) workList += proc.getSignature
		      flag
		  }
  	processed ++= workList
    while(!workList.isEmpty){
      val sig = workList.remove(0)
      println(sig)
      processed += sig
      val tmp =
			  (if (par) ps.par else ps).filter{
			    proc =>
			      var flag : Boolean = false
			      if(proc.isConcrete){
			        if(!proc.hasProcedureBody) proc.resolveBody
			        val body = proc.getProcedureBody
			        flag = body.locations.map{
			          loc =>
			            visitLoc(sig, loc)
			        }.exists(_ == true)
			      }
			      flag
			  }
      workList ++= tmp.map(_.getSignature).filter(!processed.contains(_)).toList
      result ++= tmp
    }
	  result
  }
  
	def getReachableProcedures(apiSigs : Set[String], par : Boolean) : Map[String, Set[JawaProcedure]] = {
	  var result : Map[String, Set[JawaProcedure]] = Map()
	  result ++= 
	    (if (par) apiSigs.par else apiSigs).map{
		    sig =>
		      (sig, getReachableProcedures(sig, par))
		  }
	  result
  }
	
	def getReachableProcedures(apiSig : String, par : Boolean) : Set[JawaProcedure] = {
	  var result : ISet[JawaProcedure] = isetEmpty
	  val ps : Set[JawaProcedure] = Center.getApplicationRecords.map(_.getProcedures).reduce(iunion[JawaProcedure])
	  
    val workList = mlistEmpty[String]
	  val processed = msetEmpty[String]
    workList += apiSig
    while(!workList.isEmpty){
      val sig = workList.remove(0)
      processed += sig
      val tmp =
			  (if (par) ps.par else ps).filter{
			    proc =>
			      var flag : Boolean = false
			      if(proc.isConcrete){
			        if(!proc.hasProcedureBody) proc.resolveBody
			        val body = proc.getProcedureBody
			        flag = body.locations.map{
			          loc =>
			            visitLoc(sig, loc)
			        }.exists(_ == true)
			      }
			      flag
			  }
      workList ++= tmp.map(_.getSignature).filter(!processed.contains(_)).toList
      result ++= tmp
    }
	  result
	}
	
	def visitLoc(apiSig : String, loc : LocationDecl) : Boolean = {
	  val apiRecName = Center.getRecordNameFromProcedureSignature(apiSig)
	  val apiSubSig = Center.getSubSigFromProcSig(apiSig)
	  val apiRec = Center.resolveRecord(apiRecName, Center.ResolveLevel.HIERARCHY)
	  var found = false
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
            if(apiSubSig == subSig){ 
              if(rec.isInterface){
                if(apiRec.isInterface) found = Center.getRecordHierarchy.isRecordRecursivelySubInterfaceOfIncluding(apiRec, rec)
                else found = Center.getRecordHierarchy.getAllImplementersOf(rec).contains(apiRec)
              } else {
                if(!apiRec.isInterface) found = Center.getRecordHierarchy.isRecordRecursivelySubClassOfIncluding(apiRec, rec)
              }
            }
          case "direct" | "static" =>
            found = (sig == apiSig)
        }
        false
    })
    
    visitor(loc)
    found
	}
	
	def visitLocUseSubSig(apiSubSig : String, loc : LocationDecl) : Boolean = {
	  var found = false
	  val visitor = Visitor.build({
      case t : CallJump if t.jump.isEmpty =>
        val sig = t.getValueAnnotation("signature") match {
          case Some(s) => s match {
            case ne : NameExp => ne.name.name
            case _ => ""
          }
          case None => ""
        }
        val subSig = Center.getSubSigFromProcSig(sig)
        found = (subSig == apiSubSig)
        false
    })
    visitor(loc)
    found
	}
	
//	def getAllCallerSignatures()
	
}