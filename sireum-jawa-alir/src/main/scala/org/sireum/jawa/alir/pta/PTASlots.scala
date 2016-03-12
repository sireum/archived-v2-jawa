/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa.alir.pta

import org.sireum.alir.Slot
import org.sireum.jawa.Signature
import org.sireum.jawa.FieldFQN
import org.sireum.jawa.JawaType

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
abstract class PTASlot(id: Any) extends Slot {
  def getId: Any = this.id
}

abstract class NameSlot(name: String) extends PTASlot(name) {
  override def toString = name
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
final case class VarSlot(varName: String, isBase: Boolean, isArg: Boolean) extends NameSlot(varName) {
  override def toString = {
    val sb = new StringBuilder
    if(isBase) sb.append("base:")
    if(isArg) sb.append("arg:")
    sb.append(varName)
    sb.toString().intern()
  }
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
final case class ClassSlot(classtyp: JawaType) extends NameSlot(classtyp.jawaName)

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
final case class StaticFieldSlot(fqn: FieldFQN) extends NameSlot(fqn.fqn)

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
abstract class HeapSlot(ins: Instance) extends PTASlot(ins){
  def matchWithInstance(ins: Instance): Boolean = this.ins == ins
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class FieldSlot(ins: Instance, fieldName: String) extends HeapSlot(ins){
  override def toString = ins.toString + "." + fieldName
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class ArraySlot(ins: Instance) extends HeapSlot(ins){
  override def toString = ins.toString
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class InstanceSlot(ins: Instance) extends PTASlot(ins){
  override def toString = ins.toString
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class InvokeSlot(sig: Signature, invTyp: String) extends PTASlot(sig){
  override def toString = "Invoke: " + invTyp + " " + sig
}
