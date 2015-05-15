package org.sireum.jawa.sjc.interactive

import org.sireum.jawa.sjc.ReporterImpl
import org.sireum.jawa.sjc.util.Position
import org.sireum.util._

case class Problem(pos: Position, msg: String, severityLevel: Int)

abstract class InteractiveReporter extends ReporterImpl {

  def compiler: Global

  val otherProblems: MList[Problem] = mlistEmpty

  override def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = try {
    severity.count += 1
    val problems =
      if (compiler eq null) {
        otherProblems
      } else if (pos.isDefined) {
        compiler.getCompilationUnit(pos.source.file) match {
          case Some(unit) =>
            unit.problems
          case None =>
            otherProblems
        }
      } else {
        otherProblems
      }
    problems += Problem(pos, msg, severity.id)
  } catch {
    case ex: UnsupportedOperationException =>
  }

  override def reset() {
    super.reset()
    otherProblems.clear()
  }
}