/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.jawa.alir.pta.reachingFactsAnalysis.model

import org.sireum.jawa.JawaMethod
import org.sireum.util._
import org.sireum.jawa.alir.pta.reachingFactsAnalysis._
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.JawaAlirInfoProvider
import org.sireum.jawa.alir.pta._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object NativeCallModel {
   def isNativeCall(p: JawaMethod): Boolean = p.isNative
   
   def doNativeCall(s: PTAResult, p: JawaMethod, args: List[String], retVars: Seq[String], currentContext: Context)(implicit factory: RFAFactFactory): (ISet[RFAFact], ISet[RFAFact], Boolean) = {
    var newFacts = isetEmpty[RFAFact]
    var delFacts = isetEmpty[RFAFact]
    var byPassFlag = true
        
    p.getSignature.signature match{
      case "Ljava/lang/Object;.getClass:()Ljava/lang/Class;" =>
        // algo:thisvalue.foreach {ins => set insRec's classObj field with a classIns whose type is java:lang:Class and name is same as ins's type
                 // then, create two facts (a) (retVarSlot, insRec.classObj), (b) ([insRec.classObj, "java:lang:Class.name"], concreteString(ins.typ))}
        require(args.size > 0)
        val thisSlot = VarSlot(args(0), false, true)
        val thisValue = s.pointsToSet(thisSlot, currentContext)
        thisValue.foreach{
          ins =>
            val insClasObj = ClassInstance(ins.typ, currentContext)
            newFacts += new RFAFact(VarSlot(retVars(0), false, false), insClasObj)
            val strIns = PTAConcreteStringInstance(insClasObj.getName, insClasObj.defSite)
            newFacts += (new RFAFact(FieldSlot(insClasObj, "java.lang.Class.name"), strIns))
        }
        byPassFlag = false
      case "Ljava/lang/Class;.getNameNative:()Ljava/lang/String;" =>
        // algo:thisValue.foreach.{ cIns => get value of (cIns.name") and create fact (retVar, value)}
        require(args.size > 0)
        val thisSlot = VarSlot(args(0), false, true)
        val thisValue = s.pointsToSet(thisSlot, currentContext)
        thisValue.foreach{
          cIns =>
            println(cIns + " " + cIns.getClass())
            require(cIns.isInstanceOf[ClassInstance])
            val name = cIns.asInstanceOf[ClassInstance].getName
            val strIns = PTAConcreteStringInstance(name, cIns.defSite)
              newFacts += (new RFAFact(VarSlot(retVars(0), false, false), strIns))
        }
        byPassFlag = false
      case _ =>
    }
    (newFacts, delFacts, byPassFlag)
  }
}
