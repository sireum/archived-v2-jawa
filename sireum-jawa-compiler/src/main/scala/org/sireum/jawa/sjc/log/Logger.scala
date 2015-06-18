package org.sireum.jawa.sjc.log

import org.sireum.jawa.sjc.util.Position
import java.io.File

abstract class AbstractLogger extends Logger
{
  def getLevel: Level.Value
  def setLevel(newLevel: Level.Value)
  def setTrace(flag: Int)
  def getTrace: Int
  final def traceEnabled = getTrace >= 0
  def successEnabled: Boolean
  def setSuccessEnabled(flag: Boolean): Unit

  def atLevel(level: Level.Value) = level.id >= getLevel.id
  def control(event: ControlEvent.Value, message: => String): Unit

  def logAll(events: Seq[LogEvent]): Unit
  /** Defined in terms of other methods in Logger and should not be called from them. */
  final def log(event: LogEvent)
  {
    event match
    {
      case s: Success => debug(s.msg)
      case l: Log => log(l.level, l.msg)
      case t: Trace => trace(t.exception)
      case setL: SetLevel => setLevel(setL.newLevel)
      case setT: SetTrace => setTrace(setT.level)
      case setS: SetSuccess => setSuccessEnabled(setS.enabled)
      case c: ControlEvent => control(c.event, c.msg)
    }
  }
}

object Logger
{
  private[sjc] val Null: AbstractLogger = new AbstractLogger {
    def getLevel: Level.Value = Level.Error
    def setLevel(newLevel: Level.Value) {}
    def getTrace = 0
    def setTrace(flag: Int) {}
    def successEnabled = false
    def setSuccessEnabled(flag: Boolean) {}
    def control(event: ControlEvent.Value, message: => String) {}
    def logAll(events: Seq[LogEvent]) {}
    def trace(t: Throwable) {}
    def success(message: => String) {}
    def log(level: Level.Value, message: => String) {}
  }

  def problem(cat: String, pos: Position, msg: String, sev: Severity.Value): Problem =
    new Problem
    {
      val category = cat
      val position = pos
      val message = msg
      val severity = sev
    }
}

/** This is intended to be the simplest logging interface for use by code that wants to log.
* It does not include configuring the logger. */
trait Logger {
  def debug(msg: String): Unit = log(Level.Debug, msg)
  def warn(msg: String): Unit = log(Level.Warn, msg)
  def info(msg: String): Unit = log(Level.Info, msg)
  def error(msg: String): Unit = log(Level.Error, msg)
  def trace(t: Throwable): Unit
  def log(level: Level.Value, msg: String): Unit = log(level, msg)
}

/** An enumeration defining the levels available for logging.  A level includes all of the levels
* with id larger than its own id.  For example, Warn (id=3) includes Error (id=4).*/
object Level extends Enumeration
{
  val Debug = Value(1, "debug")
  val Info = Value(2, "info")
  val Warn = Value(3, "warn")
  val Error = Value(4, "error")
  /** Defines the label to use for success messages.  
  * Because the label for levels is defined in this module, the success label is also defined here. */
  val SuccessLabel = "success"

  def union(a: Value, b: Value) = if(a.id < b.id) a else b
  def unionAll(vs: Seq[Value]) = vs reduceLeft union

  /** Returns the level with the given name wrapped in Some, or None if no level exists for that name. */
  def apply(s: String) = values.find(s == _.toString)
  /** Same as apply, defined for use in pattern matching. */
  private[sjc] def unapply(s: String) = apply(s)
}