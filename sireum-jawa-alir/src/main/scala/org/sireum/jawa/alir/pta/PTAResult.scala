/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.pta

import org.sireum.util._
import org.sireum.jawa.alir.Context
import org.sireum.alir.Slot

class PTAResult {
  private val ptMap : MMap[Context, MMap[Slot, MSet[Instance]]] = mmapEmpty
  def pointsToMap : IMap[Context, IMap[Slot, ISet[Instance]]] = {
    ptMap.map{
      case (c, m) =>
        (c, m.map{
          case (str, s) =>
            (str, s.toSet)
        }.toMap)
    }.toMap
  }
  def setInstance(s : Slot, context : Context, i : Instance) = {
    ptMap.getOrElseUpdate(context.copy, mmapEmpty).getOrElseUpdate(s, msetEmpty).clear()
    ptMap(context.copy)(s) += i
  }
  def setInstances(s : Slot, context : Context, is : ISet[Instance]) = {
    ptMap.getOrElseUpdate(context.copy, mmapEmpty).getOrElseUpdate(s, msetEmpty).clear()
    ptMap(context.copy)(s) ++= is
  }
  def addInstance(s : Slot, context : Context, i : Instance) = ptMap.getOrElseUpdate(context.copy, mmapEmpty).getOrElseUpdate(s, msetEmpty) += i
  def addInstances(s : Slot, context : Context, is : ISet[Instance]) = ptMap.getOrElseUpdate(context.copy, mmapEmpty).getOrElseUpdate(s, msetEmpty) ++= is
  def removeInstance(s : Slot, context : Context, i : Instance) = {
    ptMap.getOrElseUpdate(context.copy, mmapEmpty).getOrElseUpdate(s, msetEmpty) -= i
  }
  def removeInstances(s : Slot, context : Context, is : ISet[Instance]) = ptMap.getOrElseUpdate(context.copy, mmapEmpty).getOrElseUpdate(s, msetEmpty) --= is
  
  def pointsToSet(s : Slot, context : Context) : ISet[Instance] = {
    ptMap.getOrElse(context.copy, mmapEmpty).getOrElse(s, msetEmpty).toSet
  }
  def getPTSMap(context : Context) : IMap[Slot, ISet[Instance]] = {
    ptMap.getOrElseUpdate(context.copy, mmapEmpty).map{
      case (str, s) =>
        (str, s.toSet)
    }.toMap
  }
  
  def getRelatedInstances(s : Slot, context : Context) : ISet[Instance] = {
    val bValue = pointsToSet(s, context)
    val rhValue = getRelatedHeapInstances(bValue, context)
    bValue ++ rhValue
  }
  
  def getRelatedHeapInstances(insts : ISet[Instance], context : Context) : ISet[Instance] ={
    val worklist : MList[Instance] = mlistEmpty ++ insts
    val processed : MSet[Instance] = msetEmpty
    val result : MSet[Instance] = msetEmpty
    while(!worklist.isEmpty){
      val ins = worklist.remove(0)
      processed += ins
      val hMap = getPTSMap(context).filter{case (s, _) => s.isInstanceOf[HeapSlot] && s.asInstanceOf[HeapSlot].matchWithInstance(ins)}
      val hInss = hMap.flatMap(_._2).toSet
      result ++= hInss
      worklist ++= hInss.filter{i => !processed.contains(i)}
    }
    result.toSet
  }
}