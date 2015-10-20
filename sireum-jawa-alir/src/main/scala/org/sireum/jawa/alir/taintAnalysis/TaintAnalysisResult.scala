/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.taintAnalysis

import org.sireum.util.ISet
import org.sireum.util.IList
import org.sireum.jawa.alir.interProcedural.InterProceduralGraph
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis
import org.sireum.jawa.Signature

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait TaintDescriptor {
  def desc: String
  def typ: String
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait TaintNode {
  def getNode: InterproceduralDataDependenceAnalysis.Node
  def getDescriptor: TaintDescriptor
  def isSource: Boolean
  def isSink: Boolean
  def isSame(tn: TaintNode): Boolean
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait TaintPath {
  def getSource: TaintNode
  def getSink: TaintNode
  def getTypes: ISet[String]
  def getPath: IList[InterproceduralDataDependenceAnalysis.Edge]
  def isSame(tp: TaintPath): Boolean
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait TaintAnalysisResult {
  def getSourceNodes: ISet[TaintNode]
  def getSinkNodes: ISet[TaintNode]
  def getTaintedPaths: ISet[TaintPath]
}