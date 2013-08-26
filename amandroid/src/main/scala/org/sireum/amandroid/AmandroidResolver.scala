package org.sireum.amandroid

import org.sireum.pilar.symbol.SymbolTableProducer
import org.sireum.pilar.ast._
import org.sireum.util._
import scala.collection.GenMap
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.amandroid.util.StringFormConverter
import org.sireum.pilar.symbol.SymbolTable

/**
 * collect info from symbol table and build Center, AmandroidRecord, and AmandroidProcedure
 */

object AmandroidResolver {
  
  val DEBUG : Boolean = true
  val TITLE : String = "AmandroidResolver"
    
  val DEFAULT_TOPLEVEL_OBJECT = "[|java:lang:Object|]"
  
  /**
   * resolve given procedure code. Normally for dummyMain
   */
    
  def resolveProcedureCode(procSig : String, code : String) : AmandroidProcedure = {
    val st = Transform.getSymbolResolveResult(Set(code))
    resolveFromSTP(st.asInstanceOf[SymbolTableProducer], false)
    Center.getProcedure(procSig)
  }
  
  /**
   * try resolve given procedure container to desired level. 
   */
    
  def tryResolveProcedure(procSig : String, desiredLevel : Center.ResolveLevel.Value) : Option[AmandroidRecord] = {
    val recordName = StringFormConverter.getRecordNameFromProcedureSignature(procSig)
    tryResolveRecord(recordName, desiredLevel)
  }
    
  /**
   * resolve given procedure container to desired level. 
   */
    
  def resolveProcedure(procSig : String, desiredLevel : Center.ResolveLevel.Value) : AmandroidRecord = {
    val recordName = StringFormConverter.getRecordNameFromProcedureSignature(procSig)
    resolveRecord(recordName, desiredLevel)
  }
  
  /**
   * resolve given procedure container to desired level. 
   */
    
  def forceResolveProcedure(procSig : String, desiredLevel : Center.ResolveLevel.Value) : AmandroidRecord = {
    val recordName = StringFormConverter.getRecordNameFromProcedureSignature(procSig)
    forceResolveRecord(recordName, desiredLevel)
  }
    
  /**
   * resolve given records to desired level. 
   */
    
  def tryResolveRecord(recordName : String, desiredLevel : Center.ResolveLevel.Value) : Option[AmandroidRecord] = {
    if(AmandroidCodeSource.containsRecord(recordName)){
	    val r = desiredLevel match{
	      case Center.ResolveLevel.BODIES => resolveToBodies(recordName)
	      case Center.ResolveLevel.INTRA_PROCEDURAL => resolveToIntraProcedural(recordName)
	      case Center.ResolveLevel.NO => new AmandroidRecord().init(recordName)
	    }
	    Some(r)
    } else {
      None
    }
  }
    
  /**
   * resolve given records to desired level. 
   */
    
  def resolveRecord(recordName : String, desiredLevel : Center.ResolveLevel.Value) : AmandroidRecord = {
    desiredLevel match{
      case Center.ResolveLevel.BODIES => resolveToBodies(recordName)
      case Center.ResolveLevel.INTRA_PROCEDURAL => resolveToIntraProcedural(recordName)
      case Center.ResolveLevel.NO => new AmandroidRecord().init(recordName)
    }
  }
  
  /**
   * resolve given records to desired level. 
   */
    
  def forceResolveRecord(recordName : String, desiredLevel : Center.ResolveLevel.Value) : AmandroidRecord = {
    desiredLevel match{
      case Center.ResolveLevel.BODIES => forceResolveToBodies(recordName)
      case Center.ResolveLevel.INTRA_PROCEDURAL => forceResolveToIntraProcedural(recordName)
      case Center.ResolveLevel.NO => new AmandroidRecord().init(recordName)
    }
  }
  
  /**
   * resolve given record to body level
   */
  
  def resolveToBodies(recordName : String) : AmandroidRecord = {
    if(!Center.containsRecord(recordName) || Center.getRecord(recordName).getResolvingLevel < Center.ResolveLevel.BODIES) forceResolveToBodies(recordName)
    Center.getRecord(recordName)
  }
  
  /**
   * resolve given record to intra-procedural level
   */
  
  def resolveToIntraProcedural(recordName : String) : AmandroidRecord = {
    if(!Center.containsRecord(recordName) || Center.getRecord(recordName).getResolvingLevel < Center.ResolveLevel.INTRA_PROCEDURAL) forceResolveToIntraProcedural(recordName)
    Center.getRecord(recordName)
  }
  
  /**
   * resolve given record to body level
   */
  
  def forceResolveToBodies(recordName : String) : AmandroidRecord = {
    val code = AmandroidCodeSource.getRecordCode(recordName)
    val st = Transform.getSymbolResolveResult(Set(code))
    resolveFromSTP(st.asInstanceOf[SymbolTableProducer], false)
    Center.getRecord(recordName)
  }
  
  /**
   * resolve given record to intra-procedural level
   */
  
  def forceResolveToIntraProcedural(recordName : String) : AmandroidRecord = {
    forceResolveToBodies(recordName)
    Center.getRecord(recordName)
  }
    
  /**
   * resolve all the records, fields and procedures from symbol table producer which provided from symbol table model
   */
	
	def resolveFromSTP(stp : SymbolTableProducer, par : Boolean) = {
    if(GlobalConfig.wholeProgram && !AmandroidCodeSource.isPreLoaded) throw new RuntimeException("In whole program mode but library code did not pre-loaded, call AmandroidCodeSource.preLoad first.")
	  resolveRecords(stp, par)
	  resolveGlobalVars(stp, par)
	  resolveProcedures(stp, par)
	  if(DEBUG){
	    Center.printDetails
	  }
	}
	
	/**
	 * collect record info from symbol table
	 */
	
	def resolveRecords(stp : SymbolTableProducer, par : Boolean) = {
	  if(DEBUG) println("Doing " + TITLE + ". Resolve records parallel: " + par)
	  val col : GenMap[ResourceUri, RecordDecl] = if(par) stp.tables.recordTable.par else stp.tables.recordTable
	  val records = col.map{
	    case (uri, rd) =>
	      val recName = rd.name.name
	      val recAccessFlag =					// It can be PUBLIC ... or empty (which means no access flag class)
	        rd.getValueAnnotation("AccessFlag") match {
            case Some(exp : NameExp) =>
              exp.name.name
            case _ => ""
          }
	      val rec : AmandroidRecord = new AmandroidRecord
	      rec.init(recName)
	      rec.setAccessFlags(recAccessFlag)
	      var exs = rd.extendsClauses.map {_.name.name}.toSet
	      if((exs.isEmpty && recName != DEFAULT_TOPLEVEL_OBJECT) || rec.isInterface) exs += DEFAULT_TOPLEVEL_OBJECT
	      rec.addNeedToResolveExtends(exs)
	      rd.attributes.foreach{
	        field =>
	          val fieldSig = field.name.name
	          val fieldAccessFlag =					// It can be PRIVATE ...
			        rd.getValueAnnotation("AccessFlag") match {
		            case Some(exp : NameExp) =>
		              exp.name.name
		            case _ => ""
		          }
	          require(field.typeSpec.isDefined)
	          val fieldType = field.typeSpec.get.asInstanceOf[NamedTypeSpec].name.name
	          val f : AmandroidField = new AmandroidField
	          f.init(fieldSig, fieldType)
	          f.setAccessFlags(fieldAccessFlag)
	          f.setDeclaringRecord(rec)
	      }
	      rec
	  }
	  records.foreach(Center.addRecord(_))
	  Center.resolveRecordsRelation
	}
	
	/**
	 * collect global variables info from symbol table
	 */
	
	def resolveGlobalVars(stp : SymbolTableProducer, par : Boolean) = {
	  if(DEBUG) println("Doing " + TITLE + ". Resolve global variables parallel: " + par)
	  val col : GenMap[ResourceUri, GlobalVarDecl] = if(par) stp.tables.globalVarTable.par else stp.tables.globalVarTable
	  val ownerRelation = col.map{
	    case (uri, gvd) =>
	      val globalVarSig = gvd.name.name // e.g. @@[|java:lang:Enum.serialVersionUID|]
	      val globalVarAccessFlag = 
	        gvd.getValueAnnotation("AccessFlag") match {
			      case Some(exp : NameExp) =>
			        exp.name.name
			      case _ => ""
			    }
	      require(gvd.typeSpec.isDefined)
	      val globalVarType = gvd.typeSpec.get.asInstanceOf[NamedTypeSpec].name.name
	      val ownerName = StringFormConverter.getRecordNameFromFieldSignature(globalVarSig)
	      
	      val f : AmandroidField = new AmandroidField
	      f.init(globalVarSig, globalVarType)
	      f.setAccessFlags(globalVarAccessFlag)
	      val ownerRecord = Center.getRecord(ownerName)
	      (f, ownerRecord)
	  }
	  if(ownerRelation.isParallel) throw new RuntimeException("Doing " + TITLE + ": ownerRelation is parallel, but we are try to adding things to AmandroidRecord.")
	  ownerRelation.foreach{
	    case (f, own) =>
	      own.addField(f)
	  }
	}
	
	/**
	 * collect procedure info from symbol table
	 */
	
	def resolveProcedures(stp : SymbolTableProducer, par : Boolean) = {
	  if(DEBUG) println("Doing " + TITLE + ". Resolve procedures parallel: " + par)
	  val col : GenMap[ResourceUri, ProcedureDecl] = if(par) stp.tables.procedureAbsTable.par else stp.tables.procedureAbsTable
	  val ownerRelation = col.map{
	    case (uri, pd) =>
	      val procName = pd.name.name
	      val procSig = 
	        pd.getValueAnnotation("signature") match {
			      case Some(exp : NameExp) =>
			        exp.name.name
			      case _ => throw new RuntimeException("Doing " + TITLE + ": Can not find signature from: " + procName)
			    }
	      val procAccessFlag = 
	        pd.getValueAnnotation("Access") match {
			      case Some(exp : NameExp) =>
			        exp.name.name
			      case _ => ""
			    }
	      val ownerName =
          pd.getValueAnnotation("owner") match {
            case Some(exp : NameExp) => 
              exp.name.name
            case _ => throw new RuntimeException("Doing " + TITLE + ": Can not find owner from: " + procName)
          }
	      
	      val proc : AmandroidProcedure = new AmandroidProcedure
	      proc.init(procName, procSig)
	      proc.setAccessFlags(procAccessFlag)
	      val ownerRecord = Center.getRecord(ownerName)
	      proc.setProcedureBody(stp.procedureSymbolTableProducer(uri).asInstanceOf[ProcedureBody])
	      (proc, ownerRecord)
	  }
	  if(ownerRelation.isParallel) throw new RuntimeException("Doing " + TITLE + ": ownerRelation is parallel, but we are try to adding things to AmandroidRecord.")
	  ownerRelation.foreach{
	    case (proc, own) =>
	      own.addProcedure(proc)
	      own.setResolvingLevel(Center.ResolveLevel.BODIES)
	  }
	}
	
//	/**
//	 * resolve records relations include whole library
//	 */
//	
//	def resolveRecordsRelationWholeProgram = {
//	  
//	}

//	def resolveProcedureBody(pst : ProcedureSymbolTable, par : Boolean) = {
//	  pst.locations
//	}
	
}