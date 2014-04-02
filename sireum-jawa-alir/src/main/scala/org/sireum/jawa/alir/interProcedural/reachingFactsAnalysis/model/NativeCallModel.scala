package org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.model

import org.sireum.jawa.JawaProcedure
import org.sireum.util._
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis._
import org.sireum.jawa.Center
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.ClassInstance
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.ReachingFactsAnalysisHelper
import org.sireum.jawa.alir.JawaAlirInfoProvider

/**
 * @author Fengguo Wei & Sankardas Roy
 */
object NativeCallModel {
	 def isNativeCall(p : JawaProcedure) : Boolean = p.isNative
	 
	 def doNativeCall(s : ISet[RFAFact], p : JawaProcedure, args : List[String], retVars : Seq[String], currentContext : Context) : (ISet[RFAFact], ISet[RFAFact], Boolean) = {
	  var newFacts = isetEmpty[RFAFact]
	  var delFacts = isetEmpty[RFAFact]
	  var byPassFlag = true
	  val factMap = ReachingFactsAnalysisHelper.getFactMap(s)
	  	  
	  p.getSignature match{
	    case "Ljava/lang/Object;.getClass:()Ljava/lang/Class;" =>
	      // algo:thisvalue.foreach {ins => set insRec's classObj field with a classIns whose type is java:lang:Class and name is same as ins's type
	               // then, create two facts (a) (retVarSlot, insRec.classObj), (b) ([insRec.classObj, "java:lang:Class.name"], concreteString(ins.typ))}
	      require(args.size > 0)
          val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      thisValue.foreach{
	        ins =>
	          require(Center.hasRecord(ins.getType.typ))
	          val insRec = Center.getRecord(ins.getType.typ)
	          val insClasObj = JawaAlirInfoProvider.getClassInstance(insRec)
	          newFacts += RFAFact(VarSlot(retVars(0)), insClasObj)
	          val strIns = RFAConcreteStringInstance(insClasObj.getName, insClasObj.getDefSite)
	          newFacts += (RFAFact(FieldSlot(insClasObj, "java.lang.Class.name"), strIns))
	      }
	      byPassFlag = false
	    case "Ljava/lang/Class;.getNameNative:()Ljava/lang/String;" =>
	      // algo:thisValue.foreach.{ cIns => get value of (cIns.name") and create fact (retVar, value)}
	      require(args.size > 0)
          val thisSlot = VarSlot(args(0))
	      val thisValue = factMap.getOrElse(thisSlot, isetEmpty)
	      thisValue.foreach{
	        cIns =>
	          println(cIns + " " + cIns.getClass())
	          require(cIns.isInstanceOf[ClassInstance])
	          val name = cIns.asInstanceOf[ClassInstance].getName
	          val strIns = RFAConcreteStringInstance(name, cIns.getDefSite)
              newFacts += (RFAFact(VarSlot(retVars(0)), strIns))
	      }
	      byPassFlag = false
	    case _ =>
	  }
	  (newFacts, delFacts, byPassFlag)
	}
}