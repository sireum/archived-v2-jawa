package org.sireum.jawa.alir.interProcedural

import org.sireum.util.ISet
import org.sireum.jawa.alir.controlFlowGraph.CGNode

/**
 * Provide an Interface to let the developer get data facts corresponding
 * to each statement.
 * 
 * @author Fengguo Wei
 */
trait InterProceduralDataFlowAnalysisResult[LatticeElement] {
  def entrySet : CGNode => ISet[LatticeElement]
}