package org.sireum.jawa.alir.interProcedural

import org.sireum.jawa.alir.controlFlowGraph._
import org.sireum.jawa.alir.reachingFactsAnalysis.RFAFact

case class InterProceduralDataFlowGraph(icfg : InterproceduralControlFlowGraph[CGNode], summary : InterProceduralMonotoneDataFlowAnalysisResult[RFAFact])