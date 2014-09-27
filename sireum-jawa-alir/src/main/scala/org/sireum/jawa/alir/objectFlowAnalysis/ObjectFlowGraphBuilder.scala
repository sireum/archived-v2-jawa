/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.objectFlowAnalysis

import org.sireum.alir.AlirEdge
import org.sireum.alir.ControlFlowGraph
import org.sireum.util._
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.pilar.ast._
import org.sireum.alir.ReachingDefinitionAnalysis
import org.sireum.jawa._

abstract class ObjectFlowGraphBuilder[Node <: OfaNode, ValueSet <: NormalValueSet] {

  type Edge = AlirEdge[Node]
  
  val pstMap : MMap[ResourceUri, ProcedureSymbolTable] = mmapEmpty
  val processed : MMap[ResourceUri, PointProc] = mmapEmpty
  var cfgs : MMap[ResourceUri, ControlFlowGraph[String]] = null
  var rdas : MMap[ResourceUri, ReachingDefinitionAnalysis.Result] = null
  
  def doOFA(pst : ProcedureSymbolTable,
            cfg : ControlFlowGraph[String],
            rda : ReachingDefinitionAnalysis.Result,
            ofg : ObjectFlowGraph[Node, ValueSet])
  
  def fix(ofg : ObjectFlowGraph[Node, ValueSet]) : Unit
  
  def extendGraphWithConstructGraph(callee : ResourceUri, pi : PointI, ofg : ObjectFlowGraph[Node, ValueSet])
  
  def setProcessed(points : MList[Point], callee : ResourceUri)
 
}