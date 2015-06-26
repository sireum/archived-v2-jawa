package org.sireum.jawa

import org.sireum.util._
import org.sireum.jawa.io.Position
import org.sireum.jawa.io.NoPosition
import org.sireum.jawa.io.AbstractFile

/** Report information, warnings and errors.
 *
 * This describes the (future) external interface for issuing information, warnings and errors.
 */
trait Reporter {
  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit

  def echo(pos: Position, msg: String): Unit    = info0(pos, msg, INFO, force = true)
  def warning(pos: Position, msg: String): Unit = info0(pos, msg, WARNING, force = false)
  def error(pos: Position, msg: String): Unit   = info0(pos, msg, ERROR, force = false)

  type Severity
  val INFO: Severity
  val WARNING: Severity
  val ERROR: Severity

  def count(severity: Severity): Int
  def resetCount(severity: Severity): Unit

  def errorCount: Int   = count(ERROR)
  def warningCount: Int = count(WARNING)

  def hasErrors: Boolean   = count(ERROR) > 0
  def hasWarnings: Boolean = count(WARNING) > 0

  def reset(): Unit = {
    resetCount(INFO)
    resetCount(WARNING)
    resetCount(ERROR)
  }

  def flush(): Unit = { }
}

trait ReporterImpl extends Reporter {
  /** Informational messages. If `!force`, they may be suppressed. */
  final def info(pos: Position, msg: String, force: Boolean): Unit = info0(pos, msg, INFO, force)

  /** For sending a message which should not be labeled as a warning/error,
   *  but also shouldn't require -verbose to be visible.
   */
  def echo(msg: String): Unit = info(NoPosition, msg, force = true)

  def comment(pos: Position, msg: String): Unit = {}

  var cancelled: Boolean = false

  override def hasErrors: Boolean = super.hasErrors || cancelled

  override def reset(): Unit = {
    super.reset()
    cancelled = false
  }

  class Severity(val id: Int)(name: String) { var count: Int = 0 ; override def toString = name}
  object INFO    extends Severity(0)("INFO")
  object WARNING extends Severity(1)("WARNING")
  lazy val ERROR = new Severity(2)("ERROR")

  def count(severity: Severity): Int       = severity.count
  def resetCount(severity: Severity): Unit = severity.count = 0
}

case class Problem(pos: Position, msg: String, sev: Int)

class DefaultReporter extends ReporterImpl {
  val problems = mmapEmpty[AbstractFile, MSet[Problem]]
  def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
    severity.count += 1
    problems.getOrElseUpdate(pos.source.file, msetEmpty) += Problem(pos, msg, severity.id)
  }
}