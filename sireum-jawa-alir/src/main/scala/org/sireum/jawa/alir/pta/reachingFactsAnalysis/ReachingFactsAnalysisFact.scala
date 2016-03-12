/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa.alir.pta.reachingFactsAnalysis

import org.sireum.jawa.alir.pta.Instance
import org.sireum.jawa.alir.pta.PTASlot
import org.sireum.util._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
final case class RFAFact(slot: Int, ins: Int) {
  def this(s: PTASlot, v: Instance)(implicit factory: RFAFactFactory) = this(factory.getSlotNum(s), factory.getInstanceNum(v))
  def s(implicit factory: RFAFactFactory): PTASlot = {
    factory.getSlot(slot)
  }
  def v(implicit factory: RFAFactFactory): Instance = {
    factory.getInstance(ins)
  }
}

class RFAFactFactory {
  private val slots: MList[PTASlot] = mlistEmpty
  private val instances: MList[Instance] = mlistEmpty
  def getSlotNum(slot: PTASlot): Int = {
    var n: Int = slots.indexOf(slot)
    if(n < 0) {
      n = slots.size
      slots += slot
    }
    n
  }
  /**
   * never call it using arbitrary num
   */
  def getSlot(num: Int): PTASlot = {
    slots(num)
  }
  def getInstanceNum(ins: Instance): Int = {
    var n: Int = instances.indexOf(ins)
    if(n < 0) {
      n = instances.size
      instances += ins
    }
    n
  }
  /**
   * never call it using arbitrary num
   */
  def getInstance(num: Int): Instance = {
    instances(num)
  }
}
