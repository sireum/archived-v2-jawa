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

object PTAResult {
  type PTSMap = IMap[PTASlot, ISet[Instance]]
}

class PTAResult {
  import PTAResult._
  
  private val ptMap : MMap[Context, MMap[PTASlot, MSet[Instance]]] = mmapEmpty
  def pointsToMap : IMap[Context, PTSMap] = {
    ptMap.map{
      case (c, m) =>
        (c, m.map{
          case (str, s) =>
            (str, s.toSet)
        }.toMap)
    }.toMap
  }
  
  def merge(result: PTAResult): PTAResult = {
    result.pointsToMap.foreach {
      case (c, m) =>
        m.foreach {
          case (str, s) =>
            addInstances(str, c, s)
        }
    }
    this
  }
  
  def setInstance(s : PTASlot, context : Context, i : Instance) = {
    ptMap.getOrElseUpdate(context.copy, mmapEmpty).getOrElseUpdate(s, msetEmpty).clear()
    ptMap(context.copy)(s) += i
  }
  def setInstances(s : PTASlot, context : Context, is : ISet[Instance]) = {
    ptMap.getOrElseUpdate(context.copy, mmapEmpty).getOrElseUpdate(s, msetEmpty).clear()
    ptMap(context.copy)(s) ++= is
  }
  def addInstance(s : PTASlot, context : Context, i : Instance) = ptMap.getOrElseUpdate(context.copy, mmapEmpty).getOrElseUpdate(s, msetEmpty) += i
  def addInstances(s : PTASlot, context : Context, is : ISet[Instance]) = ptMap.getOrElseUpdate(context.copy, mmapEmpty).getOrElseUpdate(s, msetEmpty) ++= is
  def removeInstance(s : PTASlot, context : Context, i : Instance) = {
    ptMap.getOrElseUpdate(context.copy, mmapEmpty).getOrElseUpdate(s, msetEmpty) -= i
  }
  def removeInstances(s : PTASlot, context : Context, is : ISet[Instance]) = ptMap.getOrElseUpdate(context.copy, mmapEmpty).getOrElseUpdate(s, msetEmpty) --= is
  
  def pointsToSet(s : PTASlot, context : Context) : ISet[Instance] = {
    ptMap.getOrElse(context.copy, mmapEmpty).getOrElse(s, msetEmpty).toSet
  }
  def getPTSMap(context : Context) : PTSMap = {
    ptMap.getOrElseUpdate(context.copy, mmapEmpty).map{
      case (str, s) =>
        (str, s.toSet)
    }.toMap
  }
  
  def getRelatedInstances(s : PTASlot, context : Context) : ISet[Instance] = {
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