package org.sireum.jawa.pilarCodeGenerator

import org.stringtemplate.v4.STGroupFile
import java.util.ArrayList
import org.sireum.util._
import org.sireum.jawa.JawaRecord
import org.stringtemplate.v4.ST
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.Center
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.MessageCenter._
import org.sireum.jawa.JawaResolver
import java.util.Arrays
import org.sireum.jawa.NormalType
import org.sireum.jawa.util.SignatureParser


abstract class ProcedureGenerator {
  protected var currentComponent : String = null
  protected var androidClasses : Set[String] = Set()
  /**
   * Map from record to list of callback procedure
   */
  protected var callbackFunctions : Map[String, Set[String]] = Map()
  protected var conditionCounter : Int = 0
  protected var codeCounter : Int = 0
  protected val template = new STGroupFile("org/sireum/jawa/resources/pilarCodeGenerator/PilarCode.stg")
  protected val procDeclTemplate = template.getInstanceOf("ProcedureDecl")
  protected val localVarsTemplate = template.getInstanceOf("LocalVars")
  protected val bodyTemplate = template.getInstanceOf("Body")
  protected val varGen = new VariableGenerator()
  protected val localVars = new ArrayList[String]
  protected val codeFragments = new ArrayList[CodeFragmentGenerator]
  
  /**
   * map from a record to it's substitute record
   */
  protected var substituteRecordMap : IMap[String, String] = imapEmpty
  
  /**
   * Map from record to it's local variable
   */
  protected var localVarsForClasses : Map[String, String] = Map()
  
  /**
   * Set of param's record name
   */
  protected var paramRecords : Set[JawaRecord] = Set()

  /**
   * set the substituteRecordMap
   */
  def setSubstituteRecordMap(map : IMap[String, String]) = this.substituteRecordMap = map
  
  /**
	 * Registers a list of classes to be automatically scanned for Android
	 * lifecycle methods
	 * @param androidClasses The list of classes to be automatically scanned for
	 * Android lifecycle methods
	 */
  def setEntryPointClasses(androidClasses : Set[String]) = {
    this.androidClasses = androidClasses
  }
  
  def setCurrentComponent(clazz : String) = {
    this.currentComponent = clazz
  }
  
    
  def setCodeCounter(codeCtr : Int) = {
    this.codeCounter = codeCtr
  }
  
   def getCodeCounter():Int = {
    this.codeCounter
  }
  
  /**
	 * Sets the list of callback functions to be integrated into the Android
	 * lifecycle
	 * @param callbackFunctions The list of callback functions to be integrated
	 * into the Android lifecycle. This is a mapping from the Android element
	 * class (activity, service, etc.) to the list of callback methods for that
	 * element.
	 */
	def setCallbackFunctions(callbackFunctions : Map[String, Set[String]]) {
		this.callbackFunctions = callbackFunctions
	}
  
	def generate(name : String) : JawaProcedure = {
	  generate(List(), name)
	}
  
	/**
	 * generate environment with predefined methods list
	 */
  def generate(methods : List[String], name : String) : JawaProcedure = {
    val recordName = this.currentComponent
    val procedureName = recordName.substring(0, recordName.length() - 2) + "." + name + "|]"
	  val annotations = new ArrayList[ST]
	  val signature = procedureName.replaceAll("\\[\\|", "[|L").replaceAll("\\:", "/").replaceAll("\\." + name, ";." + name + ":()V")
	  initProcedureHead("[|void|]", procedureName, recordName, signature, "STATIC")
	  val code = generateInternal(List())
    msg_normal("environment code:\n" + code)
    JawaResolver.resolveProcedureCode(signature, code)
  }
  
  def generateWithParam(params : List[String], name : String) : JawaProcedure = {
    val recordName = this.currentComponent
    val procedureName = recordName.substring(0, recordName.length() - 2) + "." + name + "|]"
	  val annotations = new ArrayList[ST]
    var parSigStr : String = ""
    params.indices.foreach{i => parSigStr += params(i).replaceAll("\\[\\|", "L").replaceAll("\\:", "/").replaceAll("\\|\\]", ";")}
	  val signature = procedureName.replaceAll("\\[\\|", "[|L").replaceAll("\\:", "/").replaceAll("\\." + name, ";." + name + ":("+parSigStr+")V")
	  initProcedureHead("[|void|]", procedureName, recordName, signature, "STATIC")
    val paramArray = new ArrayList[ST]
    params.indices.foreach{
      i =>
        val paramVar = template.getInstanceOf("ParamVar")
			  val p = varGen.generate(params(i))
			  localVarsForClasses += (params(i) -> p)
			  this.paramRecords += Center.resolveRecord(params(i), Center.ResolveLevel.BODIES)
			  paramVar.add("typ", params(i))
			  paramVar.add("name", p)
			  val annot = generateExpAnnotation("type", List("object"))
			  paramVar.add("annotations", new ArrayList[ST](Arrays.asList(annot)))
			  paramArray.add(i, paramVar)
    }
    procDeclTemplate.add("params", paramArray)
    val code = generateInternal(List())
    msg_critical("environment code:\n" + code)
    JawaResolver.resolveProcedureCode(signature, code)
  }
  
  protected def generateParamAnnotation(flag : String, params : List[String]) : ST = {
    val paramArray = new ArrayList[String]
    params.foreach(param => paramArray.add(param))
    val annot = template.getInstanceOf("annotationWithParam")
	  annot.add("flag", flag)
	  annot.add("params", paramArray)
  }
  
  protected def generateExpAnnotation(flag : String, exps : List[String]) : ST = {
    val expArray = new ArrayList[String]
    exps.foreach(exp => expArray.add(exp))
    val annot = template.getInstanceOf("annotationWithExp")
	  annot.add("flag", flag)
	  annot.add("exps", expArray)
  }
  
  protected def initProcedureHead(retTyp : String, procedureName : String, owner : String, signature : String, access : String) = {
	  procDeclTemplate.add("retTyp", retTyp)
	  procDeclTemplate.add("procedureName", procedureName)
	  val annotations = new ArrayList[ST]
	  annotations.add(generateExpAnnotation("owner", List(owner)))
	  annotations.add(generateExpAnnotation("signature", List(signature)))
	  annotations.add(generateExpAnnotation("Access", List(access)))
	  procDeclTemplate.add("annotations", annotations)
  }
	
	def generateInternal(procedures : List[String]) : String
	
	protected def generateBody() : ArrayList[String] = {
	  val body : ArrayList[String] = new ArrayList[String]
	  for(i <- 0 to codeFragments.size() - 1){
	    body.add(i, codeFragments.get(i).generate)
	  }
	  body
	}
	
	protected def generateInstanceCreation(recordName : String, codefg : CodeFragmentGenerator) : String = {
	  val rhs =
		  if(recordName == "[|java:lang:String|]"){
		    val stringAnnot = generateExpAnnotation("type", List("object"))
		    "\"\" " + stringAnnot.render() 
		  } else {
			  val newExp = template.getInstanceOf("NewExp")
			  newExp.add("name", recordName)
			  newExp.render()
		  }
	  val va = varGen.generate(recordName)
	  val variable = template.getInstanceOf("LocalVar")
	  variable.add("typ", recordName)
	  variable.add("name", va)
	  localVars.add(variable.render())
	  val asmt = template.getInstanceOf("AssignmentStmt")
	  asmt.add("lhs", va)
	  asmt.add("rhs", rhs)
	  codefg.setCode(asmt)
	  va
	}

	
	def generateRecordConstructor(r : JawaRecord, constructionStack : MSet[JawaRecord], codefg : CodeFragmentGenerator) : String = {
	  constructionStack.add(r)
	  val ps = r.getProcedures
	  var cons : String = null
	  val conProcs = ps.filter(p => p.isConstructor && !p.isStatic && !p.getParamTypes.contains(NormalType("[|java:lang:Class|]", 0)))
	  if(!conProcs.isEmpty){
	    val p = conProcs.minBy(_.getParamTypes.size)
	  	cons = p.getSignature
	  }
	  if(cons != null){
	    generateProcedureCall(cons, "direct", localVarsForClasses(r.getName), constructionStack, codefg)
	  } else {
	    err_msg_normal("Warning, cannot find constructor for " + r)
	  }
	  cons
	}
	
	
	protected def generateProcedureCall(pSig : String, typ : String, localClassVar : String, constructionStack : MSet[JawaRecord], codefg : CodeFragmentGenerator) : Unit = {
	  val sigParser = new SignatureParser(pSig).getParamSig
    val paramNum = sigParser.getParameterNum
    val params = sigParser.getObjectParameters
    var paramVars : Map[Int, String] = Map()
    params.foreach{
	    case(i, param) =>
        var r = Center.resolveRecord(param.typ, Center.ResolveLevel.BODIES)
        val outterClassOpt = if(r.isInnerClass) Some(r.getOuterClass) else None
        if(!r.isConcrete){
          var substRecordName = this.substituteRecordMap.getOrElse(r.getName, null)
          if(substRecordName != null) r = Center.resolveRecord(substRecordName, Center.ResolveLevel.BODIES)
        }
        // to protect from going into dead constructor create loop
        if(!r.isConcrete){
          val va = varGen.generate(r.getName)
          localVarsForClasses += (r.getName -> va)
          paramVars += (i -> va)
          err_msg_normal("Cannot create valid constructer for " + r + ", because it is " + r.getAccessFlagString + " and cannot find substitute.")
        } else if(!constructionStack.contains(r)){
				  val va = generateInstanceCreation(r.getName, codefg)
				  localVarsForClasses += (r.getName -> va)
          paramVars += (i -> va)
          generateRecordConstructor(r, constructionStack, codefg)
        } else {
          paramVars += (i -> localVarsForClasses(r.getName))
        }
    }
    val invokeStmt = template.getInstanceOf("InvokeStmt")
    invokeStmt.add("funcName", StringFormConverter.getProcedureNameFromProcedureSignature(pSig))
    val finalParamVars : ArrayList[String] = new ArrayList[String]
    finalParamVars.add(0, localClassVar)
    for(i <- 0 to paramNum - 1){
      if(paramVars.contains(i)){
        finalParamVars.add(i + 1, paramVars(i))
      } else {
        finalParamVars.add(i + 1, "0")
      }
    }
    invokeStmt.add("params", finalParamVars)
    val annotations = new ArrayList[ST]
	  annotations.add(generateExpAnnotation("signature", List(pSig)))
	  annotations.add(generateExpAnnotation("type", List(typ)))
    invokeStmt.add("annotations", annotations)
    codefg.setCode(invokeStmt)
	}
	
	protected def generateCallToAllCallbacks(callbackRecord : JawaRecord, callbackProcedures : Set[JawaProcedure], classLocalVar : String, codefg : CodeFragmentGenerator) = {
	  var oneCallBackFragment = codefg
	  callbackProcedures.foreach{
	    callbackProcedure =>
	      val pSig = callbackProcedure.getSignature
	      val thenStmtFragment = new CodeFragmentGenerator
	      createIfStmt(thenStmtFragment, oneCallBackFragment)
	      val elseStmtFragment = new CodeFragmentGenerator
	      createGotoStmt(elseStmtFragment, oneCallBackFragment)
	      thenStmtFragment.addLabel
	      codeFragments.add(thenStmtFragment)
	      generateProcedureCall(pSig, "virtual", classLocalVar, msetEmpty + callbackRecord, thenStmtFragment)
	      elseStmtFragment.addLabel
	      codeFragments.add(elseStmtFragment)
	      oneCallBackFragment = new CodeFragmentGenerator
		    oneCallBackFragment.addLabel
		    codeFragments.add(oneCallBackFragment)
	  }
	}
	
	protected def searchAndBuildProcedureCall(subsignature : String, record : JawaRecord, entryPoints : MList[String], constructionStack : MSet[JawaRecord], codefg : CodeFragmentGenerator) = {
	  val apopt = findProcedure(record, subsignature)
	  apopt match{
	    case Some(ap) =>
	      entryPoints -= ap.getSignature
		    assert(ap.isStatic || localVarsForClasses(record.getName) != null)
		    generateProcedureCall(ap.getSignature, "virtual", localVarsForClasses(record.getName), constructionStack, codefg)
	    case None =>
	      err_msg_normal("Could not find Android entry point procedure: " + subsignature)
	      null
	  }
	}
	
	/**
	 * Generates invocation statements for all callback methods which need to be invoked during the give record's run cycle.
	 * @param rUri Current record resource uri which under process
	 * @param classLocalVar The local variable fro current record
	 */
	protected def addCallbackProcedures(record : JawaRecord, parentClassLocalVar : String, codefg : CodeFragmentGenerator) : Unit = {
	  if(!this.callbackFunctions.contains(record.getName)) return
	  var callbackRecords : Map[JawaRecord, ISet[JawaProcedure]] = Map()
    this.callbackFunctions(record.getName).map{
	    case (pSig) => 
	      val theRecord = Center.resolveRecord(StringFormConverter.getRecordNameFromProcedureSignature(pSig), Center.ResolveLevel.BODIES)
	      val theProcedure = findProcedure(theRecord, Center.getSubSigFromProcSig(pSig))
	      theProcedure match {
	        case Some(proc) =>
			      callbackRecords += (theRecord -> (callbackRecords.getOrElse(theRecord, isetEmpty) + proc))
	        case None =>
	          err_msg_normal("Could not find callback method " + pSig)
	      }
	      
	  }
	  var oneCallBackFragment = codefg
		callbackRecords.foreach{
		  case(callbackRecord, callbackProcedures) =>
		    val classLocalVar : String =
		      if(isCompatible(record, callbackRecord)) parentClassLocalVar
		      // create a new instance of this class
		      else{
			      val va = generateInstanceCreation(callbackRecord.getName, oneCallBackFragment)
		        this.localVarsForClasses += (callbackRecord.getName -> va)
		        generateRecordConstructor(callbackRecord, msetEmpty + record, oneCallBackFragment)
		        va
		      }
		    if(classLocalVar != null){
		      // build the calls to all callback procedures in this record
		      generateCallToAllCallbacks(callbackRecord, callbackProcedures, classLocalVar, oneCallBackFragment)
		    } else {
		      err_msg_normal("Constructor cannot be generated for callback class " + callbackRecord)
		    }
		    oneCallBackFragment = new CodeFragmentGenerator
		    oneCallBackFragment.addLabel
		    codeFragments.add(oneCallBackFragment)
		}
	}
	
	protected def isCompatible(actual : JawaRecord, expected : JawaRecord) : Boolean = {
	  var act : JawaRecord = actual
	  while(act != null){
	    if(act.getName.equals(expected.getName))
	      return true
	    if(expected.isInterface)
	      act.getInterfaces.foreach{int => if(int.getName.equals(expected.getName)) return true}
	    if(!act.hasSuperClass)
	      act = null
	    else act = act.getSuperClass
	  }
	  false
	}
	
	protected def createIfStmt(targetfg : CodeFragmentGenerator, codefg : CodeFragmentGenerator) = {
	  val target = targetfg.getLabel
	  if(target != null){
	    val condExp = template.getInstanceOf("CondExp")
      condExp.add("lhs", "RandomCoinToss")
      condExp.add("rhs", "head")
      val ifStmt = template.getInstanceOf("IfStmt")
      ifStmt.add("cond", condExp)
      ifStmt.add("label", target)
      codefg.setCode(ifStmt)
	  }
	}
	
	protected def createGotoStmt(targetfg : CodeFragmentGenerator, codefg : CodeFragmentGenerator) = {
	  val target = targetfg.getLabel
	  if(target != null){
      val gotoStmt = template.getInstanceOf("GotoStmt")
      gotoStmt.add("label", target)
      codefg.setCode(gotoStmt)
	  }
	}
	
	protected def createReturnStmt(variable : String, codefg : CodeFragmentGenerator) = {
	  val returnStmt = template.getInstanceOf("ReturnStmt")
      returnStmt.add("variable", variable)
      codefg.setCode(returnStmt)
	}
	
	protected def createFieldSetStmt(base : String, field : String, rhs : String, annoTyps : List[String], codefg : CodeFragmentGenerator) = {
    val mBaseField = template.getInstanceOf("FieldAccessExp")
	  mBaseField.add("base", base)
	  mBaseField.add("field", field)
	  val asmt = template.getInstanceOf("AssignmentStmt")
	  asmt.add("lhs", mBaseField)
	  asmt.add("rhs", rhs)
	  val annos = generateExpAnnotation("type", annoTyps)
	  asmt.add("annotations", annos)
	  codefg.setCode(asmt)
	}
	
	protected class CodeFragmentGenerator {
	  protected val codeFragment = template.getInstanceOf("CodeFragment")
	  protected val codes : ArrayList[ST] = new ArrayList[ST]
	  protected var label = template.getInstanceOf("Label")
	  
	  def addLabel() = {
	    label.add("num", conditionCounter)
	    codeFragment.add("label", label)
	    conditionCounter += 1
	  }
	  def getLabel() : ST = label
	  def setCode(code : ST) = {
	    codes.add(code)
	  }
	  def generate() : String = {
	    val finalCodes = new ArrayList[ST]
	    for(i <- 0 to codes.size - 1){
	      val code = template.getInstanceOf("Code")
	      code.add("num", codeCounter)
	      codeCounter += 1
	      code.add("code", codes.get(i))
	      finalCodes.add(i, code)
	    }
	    codeFragment.add("codes", finalCodes)
	    codeFragment.render()
	  }
	}
	
	protected def findProcedure(currentRecord : JawaRecord, subSig : String) : Option[JawaProcedure] = {
	  if(currentRecord.declaresProcedure(subSig)) Some(currentRecord.getProcedure(subSig))
	  else if(currentRecord.hasSuperClass) findProcedure(currentRecord.getSuperClass, subSig)
	  else None
	}
}