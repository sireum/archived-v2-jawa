package org.sireum.jawa

import org.sireum.pilar.symbol.SymbolTableProducer
import org.sireum.pilar.ast._
import org.sireum.util._
import scala.collection.GenMap
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.jawa.util.StringFormConverter
import org.sireum.pilar.symbol.SymbolTable
import org.sireum.jawa.MessageCenter._


/**
 * this object collects info from the symbol table and builds Center, JawaRecord, and JawaProcedure
 */

object JawaResolver {
  
  val DEBUG : Boolean = false
  val TITLE : String = "AmandroidResolver"
  
  /**
   * resolve the given procedure code. Normally for dummyMain 
   */
    
  def resolveProcedureCode(procSig : String, code : String) : JawaProcedure = {
    val st = Transform.getSymbolResolveResult(Set(code))
    resolveFromST(st, Center.ResolveLevel.BODY, GlobalConfig.jawaResolverParallel)
    Center.getProcedureWithoutFailing(procSig)
  }
  
  /**
   * resolve the given procedure's body to body level. 
   */
    
  def resolveProcedureBody(procSig : String) : ProcedureBody = {
    val code = JawaCodeSource.getProcedureCodeWithoutFailing(procSig)
    val st = Transform.getSymbolResolveResult(Set(code))
    st.procedureSymbolTables.foreach{
      pst =>
        val sig = 
	        pst.procedure.getValueAnnotation("signature") match {
			      case Some(exp : NameExp) =>
			        exp.name.name
			      case _ => throw new RuntimeException("Doing " + TITLE + ": Can not find signature from: " + pst.procedureUri)
			    }
        if(procSig == sig) return pst.asInstanceOf[ProcedureBody]
    }
    throw new RuntimeException("Doing " + TITLE + ": Can not resolve procedure body for " + procSig)
  }
    
  /**
   * resolve the given records to desired level. 
   */
    
  def tryResolveRecord(recordName : String, desiredLevel : Center.ResolveLevel.Value) : Option[JawaRecord] = {
    if(JawaCodeSource.containsRecord(recordName)){
	    val r = desiredLevel match{
	      case Center.ResolveLevel.BODY => resolveToBody(recordName)
	      case Center.ResolveLevel.HIERARCHY => resolveToHierarchy(recordName)
	    }
	    Some(r)
    } else {
      None
    }
  }
    
  /**
   * resolve the given records to desired level. 
   */
    
  def resolveRecord(recordName : String, desiredLevel : Center.ResolveLevel.Value) : JawaRecord = {
    val typ = StringFormConverter.getTypeFromName(recordName)
    if(!typ.isArray && !JawaCodeSource.containsRecord(recordName)){
      if(!Center.containsRecord(recordName) || Center.getRecord(recordName).getResolvingLevel < desiredLevel){
	      val rec = new JawaRecord().init(recordName)
	      rec.setPhantom
	      rec.setResolvingLevel(desiredLevel)
	      Center.tryRemoveRecord(recordName)
	      Center.addRecord(rec)
	      msg_detail("add phantom record " + rec)
	      rec
      } else Center.getRecord(recordName)
    } else {
	    desiredLevel match{
	      case Center.ResolveLevel.BODY => resolveToBody(recordName)
	      case Center.ResolveLevel.HIERARCHY => resolveToHierarchy(recordName)
	    }
    }
  }
  
  /**
   * resolve the given records to desired level. 
   */
    
  def forceResolveRecord(recordName : String, desiredLevel : Center.ResolveLevel.Value) : JawaRecord = {
    desiredLevel match{
      case Center.ResolveLevel.BODY => forceResolveToBody(recordName)
      case Center.ResolveLevel.HIERARCHY => forceResolveToHierarchy(recordName)
    }
  }
  
  /**
   * resolve the given record to hierarchy level
   */
  
  def resolveToHierarchy(recordName : String) : JawaRecord = {
    if(!Center.containsRecord(recordName) || Center.getRecord(recordName).getResolvingLevel < Center.ResolveLevel.HIERARCHY) forceResolveToHierarchy(recordName)
    Center.getRecord(recordName)
  }
  
  /**
   * force resolve the given record to hierarchy level
   */
  
  private def forceResolveToHierarchy(recordName : String) : JawaRecord = {
    val typ = StringFormConverter.getTypeFromName(recordName)
    if(typ.isArray){
      resolveArrayRecord(typ)
    } else {
	    val code = JawaCodeSource.getRecordCode(recordName, Center.ResolveLevel.HIERARCHY)
	    val st = Transform.getSymbolResolveResult(Set(code))
	    Center.tryRemoveRecord(recordName)
	    resolveFromST(st, Center.ResolveLevel.HIERARCHY, GlobalConfig.jawaResolverParallel)
    }
    Center.getRecord(recordName)
  }
  
  /**
   * resolve the given record to body level
   */
  
  def resolveToBody(recordName : String) : JawaRecord = {
    if(!Center.containsRecord(recordName)) forceResolveToBody(recordName)
    else if(Center.getRecord(recordName).getResolvingLevel < Center.ResolveLevel.BODY) escalateReolvingLevel(Center.getRecord(recordName), Center.ResolveLevel.BODY)
    else Center.getRecord(recordName)
  }
  
  /**
   * escalate resolving level
   */
  
  private def escalateReolvingLevel(rec : JawaRecord, desiredLevel : Center.ResolveLevel.Value) : JawaRecord = {
    require(rec.getResolvingLevel < desiredLevel)
    if(desiredLevel == Center.ResolveLevel.BODY){
      rec.getProcedures.foreach(_.tryResolveBody)
      rec.setResolvingLevel(Center.ResolveLevel.BODY)
    }
    rec
  }
  
  /**
   * force resolve the given record to body level
   */
  
  private def forceResolveToBody(recordName : String) : JawaRecord = {
    val typ = StringFormConverter.getTypeFromName(recordName)
    if(typ.isArray){
      resolveArrayRecord(typ)
    } else {
	    val code = JawaCodeSource.getRecordCode(recordName, Center.ResolveLevel.BODY)
	    val st = Transform.getSymbolResolveResult(Set(code))
	    Center.tryRemoveRecord(recordName)
	    resolveFromST(st, Center.ResolveLevel.BODY, GlobalConfig.jawaResolverParallel)
    }
    Center.getRecord(recordName)
  }
  
  /**
   * resolve array record
   */
  
  def resolveArrayRecord(typ : Type) : Unit = {
    val recName = typ.name
    val recAccessFlag =	
      if(Center.isJavaPrimitiveType(typ.typ)){
      	"FINAL_PUBLIC"
	    } else {
	      val base = resolveRecord(typ.typ, Center.ResolveLevel.HIERARCHY)
	      val baseaf = base.getAccessFlagString
	      if(baseaf.contains("FINAL")) baseaf else "FINAL_" + baseaf
	    }
    val rec : JawaRecord = new JawaRecord
    rec.init(recName)
    rec.setAccessFlags(recAccessFlag)
    rec.addNeedToResolveExtends(Set(Center.DEFAULT_TOPLEVEL_OBJECT))
    if(Center.isInnerClassName(recName)) rec.needToResolveOuterName = Some(Center.getOuterNameFrom(recName))
    rec.setResolvingLevel(Center.ResolveLevel.BODY)
    Center.addRecord(rec)
    rec.addField(createClassField(rec))
	  Center.resolveRecordsRelationWholeProgram
  }
    
  /**
   * resolve all the records, fields and procedures from symbol table producer which are provided from symbol table model
   */
	
	def resolveFromST(st : SymbolTable, level : Center.ResolveLevel.Value, par : Boolean) : Unit = {
    if(!JawaCodeSource.isPreLoaded) throw new RuntimeException("In whole program mode but library code did not been pre-loaded, call AmandroidCodeSource.preLoad first.")
    val stp = st.asInstanceOf[SymbolTableProducer]
	  resolveRecords(stp, level, par)
	  resolveGlobalVars(stp, level, par)
	  resolveProcedures(stp, level, par)
	  if(DEBUG){
	    Center.printDetails
	  }
	}
	
	/**
	 * collect record info from symbol table
	 */
	
	def resolveRecords(stp : SymbolTableProducer, level : Center.ResolveLevel.Value, par : Boolean) = {
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
	      val rec : JawaRecord = new JawaRecord
	      rec.init(recName)
	      rec.setAccessFlags(recAccessFlag)
	      var exs = rd.extendsClauses.map {_.name.name}.toSet
	      rec.addNeedToResolveExtends(exs)
	      if(Center.isInnerClassName(recName)) rec.needToResolveOuterName = Some(Center.getOuterNameFrom(recName))
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
	          var d = 0
			      var tmpTs = field.typeSpec.get
			      while(tmpTs.isInstanceOf[SeqTypeSpec]){
		          d += 1
		          tmpTs = tmpTs.asInstanceOf[SeqTypeSpec].elementType
		        }
			      require(tmpTs.isInstanceOf[NamedTypeSpec])
			      val fieldType : NormalType = new NormalType(tmpTs.asInstanceOf[NamedTypeSpec].name.name, d)
	          val f : JawaField = new JawaField
	          f.init(fieldSig, fieldType)
	          f.setAccessFlags(fieldAccessFlag)
	          rec.addField(f)
	      }
	      rec.setResolvingLevel(level)
	      rec
	  }.toSet
	  records.foreach(Center.addRecord(_))
	  Center.resolveRecordsRelationWholeProgram
//	  else Center.resolveRecordsRelation
	  // now we generate a special Amandroid Procedure for each record; this proc would represent the const-class operation
	  records.foreach{
	    rec =>
	      rec.addField(createClassField(rec))
	  }
	}
	
	private def createClassField(rec : JawaRecord) : JawaField = {
	  val field : JawaField = new JawaField
    val fSig = StringFormConverter.generateFieldSignature(rec.getName, "class", false)
    field.init(fSig, NormalType("[|java:lang:Class|]", 0))
    field.setAccessFlags("FINAL_STATIC")
    field
	}
	
	/**
	 * collect global variables info from the symbol table
	 */
	
	def resolveGlobalVars(stp : SymbolTableProducer, level : Center.ResolveLevel.Value, par : Boolean) = {
	  if(DEBUG) println("Doing " + TITLE + ". Resolve global variables parallel: " + par)
	  val col : GenMap[ResourceUri, GlobalVarDecl] = if(par) stp.tables.globalVarTable.par else stp.tables.globalVarTable
	  val ownerRelation = col.map{
	    case (uri, gvd) =>
	      val globalVarSig = gvd.name.name // e.g. @@[|java:lang:Enum.serialVersionUID|]
	      import org.sireum.pilar.symbol.Symbol.pp2r
	      Center.setGlobalVarSigToUri(gvd.name.name, gvd.name.uri)
	      val globalVarAccessFlag = 
	        gvd.getValueAnnotation("AccessFlag") match {
			      case Some(exp : NameExp) =>
			        exp.name.name
			      case _ => ""
			    }
	      require(gvd.typeSpec.isDefined)
	      var d = 0
	      var tmpTs = gvd.typeSpec.get
	      while(tmpTs.isInstanceOf[SeqTypeSpec]){
          d += 1
          tmpTs = tmpTs.asInstanceOf[SeqTypeSpec].elementType
        }
	      require(tmpTs.isInstanceOf[NamedTypeSpec])
	      val globalVarType : NormalType = new NormalType(tmpTs.asInstanceOf[NamedTypeSpec].name.name, d)
	      val ownerName = StringFormConverter.getRecordNameFromFieldSignature(globalVarSig)
	      
	      val f : JawaField = new JawaField
	      f.init(globalVarSig, globalVarType)
	      f.setAccessFlags(globalVarAccessFlag)
	      val ownerRecord = Center.getRecord(ownerName)
	      (f, ownerRecord)
	  }
	  if(ownerRelation.isParallel) throw new RuntimeException("Doing " + TITLE + ": ownerRelation is parallel, but we are trying to add things to JawaRecord.")
	  ownerRelation.foreach{
	    case (f, own) =>
	      own.addField(f)
	  }
	}
	
	/**
	 * collect procedure info from symbol table
	 */
	
	def resolveProcedures(stp : SymbolTableProducer, level : Center.ResolveLevel.Value, par : Boolean) = {
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
	      val paramNames = pd.params.map{_.name.name}.toList
	      val proc : JawaProcedure = new JawaProcedure
	      proc.init(procName, procSig)
	      proc.setAccessFlags(procAccessFlag)
	      proc.setParameterNames(paramNames)
	      proc.setResolvingLevel(level)
	      val ownerRecord = Center.getRecord(ownerName)
	      if(level >= Center.ResolveLevel.BODY){
	      	proc.setProcedureBody(stp.procedureSymbolTableProducer(uri).asInstanceOf[ProcedureBody])
		      if(pd.body.isInstanceOf[ImplementedBody]){
		        val body = pd.body.asInstanceOf[ImplementedBody]
		        val catchclauses = body.catchClauses
		        catchclauses.foreach{
		          catchclause =>
		            require(catchclause.typeSpec.isDefined)
		            require(catchclause.typeSpec.get.isInstanceOf[NamedTypeSpec])
		            val excName = catchclause.typeSpec.get.asInstanceOf[NamedTypeSpec].name.name
			          proc.addExceptionHandler(excName, catchclause.fromTarget.name, catchclause.toTarget.name, catchclause.jump.target.name)
		        }
		      }
	      }
	      (proc, ownerRecord)
	  }
	  if(ownerRelation.isParallel) throw new RuntimeException("Doing " + TITLE + ": ownerRelation is parallel, but we are trying to add things to JawaRecord.")
	  ownerRelation.foreach{
	    case (proc, own) =>
	      own.addProcedure(proc)
	  }
	}
	
}