package org.sireum.jawa.alir.interProcedural

import org.sireum.jawa.alir.interProcedural.controlFlowGraph._
import org.sireum.jawa.alir.interProcedural.reachingFactsAnalysis.RFAFact

case class InterProceduralDataFlowGraph(icfg : InterproceduralControlFlowGraph[CGNode], summary : InterProceduralMonotoneDataFlowAnalysisResult[RFAFact])