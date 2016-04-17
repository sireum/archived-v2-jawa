package org.sireum.jawa.alir.apicheck

import org.sireum.jawa.Global
import org.sireum.jawa.alir.dataFlowAnalysis.InterProceduralDataFlowGraph
import org.sireum.util._
import org.sireum.jawa.alir.Context

trait ApiMisuseChecker {
  def check(global: Global, idfg: InterProceduralDataFlowGraph): ApiMisuseResult
}

case class ApiMisuseResult(misusedApis: IMap[Context, String]) {
  def print: Unit = misusedApis.foreach {
    case (c, des) => println(c + " " + des)
  }
}