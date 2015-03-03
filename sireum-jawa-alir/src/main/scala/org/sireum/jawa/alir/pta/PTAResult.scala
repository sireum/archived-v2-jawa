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

trait PTAResult {
  private val ptMap : MMap[(String, Context), MSet[PTAInstance]] = mmapEmpty
  def pointsToMap : IMap[(String, Context), MSet[PTAInstance]] = ptMap.toMap
  def setInstance(key : String, context : Context, i : PTAInstance) = ptMap((key, context)) = msetEmpty + i
  def setInstances(key : String, context : Context, is : MSet[PTAInstance]) = ptMap((key, context)) = is
  def addInstance(key : String, context : Context, i : PTAInstance) : Boolean = ptMap.getOrElseUpdate((key, context), msetEmpty).add(i)
  def addInstances(key : String, context : Context, is : MSet[PTAInstance]) = ptMap.getOrElseUpdate((key, context), msetEmpty) ++= is
  def removeInstance(key : String, context : Context, i : PTAInstance) : Boolean = ptMap.getOrElseUpdate((key, context), msetEmpty).remove(i)
  def removeInstances(key : String, context : Context, is : MSet[PTAInstance]) = ptMap.getOrElseUpdate((key, context), msetEmpty) --= is
  
  def pointsToSet(key : String, context : Context) : MSet[PTAInstance] = {
    ptMap.getOrElse((key, context), msetEmpty)
  }
}