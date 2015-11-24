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
import org.sireum.alir.AlirEdge
import org.sireum.jawa.alir.interProcedural.InterProceduralNode

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
case class TypeTaintDescriptor(desc: String, position: Option[Int], typ: String) extends TaintDescriptor {
  override def toString: String = s"$typ: $desc ${if(position.isDefined) position.get.toString else ""}"
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
case class TagTaintDescriptor(desc: String, positions: ISet[Int], typ: String, tags: ISet[String]) extends TaintDescriptor {
  override def toString: String = s"$typ: $desc ${positions.mkString("|")} ${tags.mkString("|")}"
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
case class TaintSource[N <: InterProceduralNode](node: N, descriptor: TaintDescriptor) {
  def isSource = true
  def isSink = false
  def isSame(tn: TaintSource[N]): Boolean = descriptor == tn.descriptor && node.getContext.getCurrentLocUri == node.getContext.getCurrentLocUri
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
case class TaintSink[N <: InterProceduralNode](node: N, descriptor: TaintDescriptor) {
  def isSource = false
  def isSink = true
  def isSame(tn: TaintSink[N]): Boolean = descriptor == descriptor && node.getContext.getCurrentLocUri == node.getContext.getCurrentLocUri
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait TaintPath[N <: InterProceduralNode, E <: AlirEdge[N]] {
  def getSource: TaintSource[N]
  def getSink: TaintSink[N]
  def getTypes: ISet[String]
  def getPath: IList[E]
  def isSame(tp: TaintPath[N, E]): Boolean
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait TaintAnalysisResult[N <: InterProceduralNode, E <: AlirEdge[N]] {
  def getSourceNodes: ISet[TaintSource[N]]
  def getSinkNodes: ISet[TaintSink[N]]
  def getTaintedPaths: ISet[TaintPath[N, E]]
}