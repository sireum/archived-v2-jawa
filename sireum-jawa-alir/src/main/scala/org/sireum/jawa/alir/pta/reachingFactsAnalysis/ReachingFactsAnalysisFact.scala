/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.pta.reachingFactsAnalysis

import org.sireum.alir.DefRef
import org.sireum.alir.Slot
import org.sireum.jawa.alir.pta.Instance
import org.sireum.util._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class VarSlot(varName : String) extends Slot {
  def isGlobal : Boolean = varName.startsWith("@@")
  override def toString = varName
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
abstract class HeapSlot(ins : Instance) extends Slot{
  def matchWithInstance(ins : Instance) : Boolean = this.ins == ins
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class FieldSlot(ins : Instance, fieldName : String) extends HeapSlot(ins){
  override def toString = ins.toString + "." + fieldName
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class ArraySlot(ins : Instance) extends HeapSlot(ins){
  override def toString = ins.toString
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class RFAFact(s : Slot, v : Instance)