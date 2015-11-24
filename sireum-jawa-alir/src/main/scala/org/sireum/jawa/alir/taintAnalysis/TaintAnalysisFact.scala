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
import org.sireum.jawa.alir.Context

object TaintSlotPosition extends Enumeration {
  val LHS, RHS, ARG = Value
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
trait TaintSlot extends Slot {
  def context: Context
  def pos: TaintSlotPosition.Value
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
final case class InstanceTaintSlot(s: PTASlot, pos: TaintSlotPosition.Value, context: Context, ins: Instance) extends TaintSlot

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
final case class PrimitiveTaintSlot(s: PTASlot, pos: TaintSlotPosition.Value, context: Context) extends TaintSlot

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
final case class TaintFact(s : TaintSlot, tag : String){
  def getContext: Context = s.context
  override def toString : String = {
    "TaintFact" + "(" + s + ":" + tag + ")"
  }
}