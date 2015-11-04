/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.taintAnalysis

import org.sireum.jawa.alir.pta.PTASlot
import org.sireum.jawa.alir.pta.Instance
import org.sireum.jawa.alir.pta.VarSlot
import org.sireum.alir.Slot
import org.sireum.jawa.alir.pta.NameSlot

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait TaintSlot extends Slot

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
final case class InstanceTaintSlot(ins: Instance) extends TaintSlot

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
final case class PrimitiveTaintSlot(s: PTASlot) extends TaintSlot

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
final case class TaintFact(s : TaintSlot, tag : String){
  override def toString : String = {
    "TaintFact" + "(" + s + ":" + tag + ")"
  }
}