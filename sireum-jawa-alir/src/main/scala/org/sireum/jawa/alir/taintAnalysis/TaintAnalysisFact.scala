/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.taintAnalysis

import org.sireum.alir.Slot
import org.sireum.jawa.alir.reachingFactsAnalysis.RFAFact

final case class TaintFact(fact : RFAFact, source : String){
  override def toString : String = {
    "TaintFact" + "(" + fact + "->" + source + ")"
  }
}