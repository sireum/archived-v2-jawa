package org.sireum.jawa.alir.taintAnalysis

import org.sireum.util.ISet
import org.sireum.util.IList
import org.sireum.jawa.alir.interProcedural.InterProceduralGraph
import org.sireum.jawa.alir.dataDependenceAnalysis.InterproceduralDataDependenceAnalysis

trait TaintDescriptor {
  def name : String
  def typ : String
}

trait TaintNode {
  def getNode : InterproceduralDataDependenceAnalysis.Node
  def getDescriptors : ISet[TaintDescriptor]
  def isSource : Boolean
  def isSink : Boolean
  def isSame(tn : TaintNode) : Boolean
}

trait TaintPath {
  def getSource : TaintNode
  def getSink : TaintNode
  def getTypes : ISet[String]
  def getPath : IList[InterproceduralDataDependenceAnalysis.Edge]
  def isSame(tp : TaintPath) : Boolean
}

trait TaintAnalysisResult {
	def getSourceNodes : ISet[TaintNode]
	def getSinkNodes : ISet[TaintNode]
	def getTaintedPaths : ISet[TaintPath]
}