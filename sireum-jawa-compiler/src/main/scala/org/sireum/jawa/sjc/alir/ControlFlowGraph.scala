package org.sireum.jawa.sjc.alir

import org.sireum.alir.AlirIntraProceduralGraph
import org.sireum.alir.AlirSuccPredAccesses
import org.sireum.alir.AlirIntraProceduralNode
import org.sireum.alir.AlirEdge
import org.sireum.jawa.sjc.parser.{Location => JawaLocation, _}
import org.sireum.util._
import org.sireum.alir.AlirEdgeAccesses
import org.sireum.jawa.sjc.parser.GotoStatement

trait ControlFlowGraph[VirtualLabel]
    extends AlirIntraProceduralGraph[ControlFlowGraph.Node, VirtualLabel]
    with AlirSuccPredAccesses[ControlFlowGraph.Node] {

  def entryNode : ControlFlowGraph.Node
  def exitNode : ControlFlowGraph.Node
  def reverse : ControlFlowGraph[VirtualLabel]
}

/**
 * @author <a href="mailto:robby@k-state.edu">Robby</a>
 */
object ControlFlowGraph {
  val BRANCH_PROPERTY_KEY = "BRANCH"
  type Node = AlirIntraProceduralNode
  type Edge = AlirEdge[Node]
  type ShouldIncludeFlowFunction = (JawaLocation, Iterable[CatchClause]) => (Iterable[CatchClause], Boolean)
//  val defaultSiff : ShouldIncludeFlowFunction = { (_, _) => (Array.empty[CatchClause], false) }

  val defaultSiff : ShouldIncludeFlowFunction =
    { (loc, catchclauses) => 
        val result = catchclauses        
        (result, false)
    } 
  
  def apply[VirtualLabel] = build[VirtualLabel] _

  def build[VirtualLabel] //
  (md : MethodDeclaration,
   entryLabel : VirtualLabel, exitLabel : VirtualLabel,
   pool : AlirIntraProceduralGraph.NodePool,
   shouldIncludeFlow : ShouldIncludeFlowFunction = defaultSiff) // 
   : ControlFlowGraph[VirtualLabel] = {

    val body = md.body match {
      case rb: ResolvedBody => rb
      case ub: UnresolvedBody => ub.resolve
    }
    val locationDecls = body.locations.toSeq
    val result = new Cfg[VirtualLabel](pool)
    if (locationDecls.isEmpty) return result

      def getLocUriIndex(l : JawaLocation) =
        (l.locationUri, l.locationIndex)

      def getNode(l : JawaLocation) =
        result.getNode(Some(l.locationUri), l.locationIndex)

    val verticesMap = mmapEmpty[ResourceUri, Node]
    for (ld <- locationDecls) {
      val lui = getLocUriIndex(ld)
      val n = result.addNode(Some(lui._1), lui._2)
      verticesMap(lui._1) = n
    }

    val exitNode = result.addVirtualNode(exitLabel)
    result.entryNode = result.addVirtualNode(entryLabel)
    result.addEdge(result.entryNode, getNode(locationDecls(0)))
    result.exitNode = exitNode
    var source : Node = null
    var next : Node = null
    
    val size = locationDecls.size
    for (i <- 0 until size) {
      val l = locationDecls(i)
      source = getNode(l)
      next = if (i != size - 1) getNode(locationDecls(i + 1)) else null
      l.statement match {
        case cs: CallStatement =>
          result.addEdge(source, next)
        case as: AssignmentStatement =>
          result.addEdge(source, next)
        case ts: ThrowStatement =>
          result.addEdge(source, exitNode)
        case is: IfStatement =>
          next = verticesMap(is.targetLocation.location)
          result.addEdge(source, next)
        case gs: GotoStatement =>
          next = verticesMap(gs.targetLocation.location)
          result.addEdge(source, next)
        case ss: SwitchStatement =>
          ss.cases foreach {
            c =>
              next = verticesMap(c.targetLocation.location)
              result.addEdge(source, next)
          }
          ss.defaultCaseOpt match {
            case Some(d) =>
              next = verticesMap(d.targetLocation.location)
              result.addEdge(source, next)
            case None => result.addEdge(source, next)
          }
        case rs: ReturnStatement =>
          result.addEdge(source, exitNode)
        case ms: MonitorStatement =>
          result.addEdge(source, next)
        case es: EmptyStatement =>
          result.addEdge(source, next)
        case _ =>
          result.addEdge(source, next)
      }
      if (shouldIncludeFlow ne defaultSiff) {
        val (ccs, toExit) = shouldIncludeFlow(l, body.getCatchClauses(l.locationSymbol.locationIndex))
        ccs.foreach { cc =>
          result.addEdge(source, verticesMap(cc.targetLocation.location))
        }
        if (toExit) result.addEdge(source, exitNode)
      }
    }

//    print(result)

    result
  }

  private class Cfg[VirtualLabel] //
  (val pool : AlirIntraProceduralGraph.NodePool)
      extends ControlFlowGraph[VirtualLabel]
      with AlirEdgeAccesses[Node] {

    var entryNode : Node = null

    var exitNode : Node = null

    def reverse : Cfg[VirtualLabel] = {
      val result = new Cfg[VirtualLabel](pool)
      for (n <- nodes) result.addNode(n)
      for (e <- edges) result.addEdge(e.target, e.source)
      result.entryNode = exitNode
      result.exitNode = entryNode
      result
    }

    override def toString = {
      val sb = new StringBuilder("CFG\n")

      for (n <- nodes)
        for (m <- successors(n)) {
          for (e <- getEdges(n, m)) {
            val branch = if (e ? ControlFlowGraph.BRANCH_PROPERTY_KEY)
              e(ControlFlowGraph.BRANCH_PROPERTY_KEY).toString
            else ""
            sb.append("%s -> %s %s\n".format(n, m, branch))
          }
        }

      sb.append("\n")

      sb.toString
    }
  }
}