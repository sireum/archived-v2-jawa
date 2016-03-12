/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa

import org.sireum.util._
import org.sireum.jawa.io.Position
import org.sireum.jawa.io.NoPosition
import org.sireum.jawa.io.AbstractFile
import java.io.FileWriter
import java.io.PrintWriter
import java.io.BufferedWriter

/** Report information, warnings and errors.
 *
 * This describes the (future) external interface for issuing information, warnings and errors.
 */
trait Reporter {
  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit
  protected def info1(title: String, msg: String, severity: Severity, force: Boolean): Unit

  def echo(pos: Position, msg: String): Unit    = info0(pos, msg, INFO, force = true)
  def warning(pos: Position, msg: String): Unit = info0(pos, msg, WARNING, force = false)
  def error(pos: Position, msg: String): Unit   = info0(pos, msg, ERROR, force = false)
  
  def echo(title: String, msg: String): Unit    = info1(title, msg, INFO, force = true)
  def warning(title: String, msg: String): Unit = info1(title, msg, WARNING, force = false)
  def error(title: String, msg: String): Unit   = info1(title, msg, ERROR, force = false)

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
  
  /** Informational messages. If `!force`, they may be suppressed. */
  final def info(title: String, msg: String, force: Boolean): Unit = info1(title, msg, INFO, force)

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
case class Problem1(title: String, msg: String, sev: Int)

class DefaultReporter extends ReporterImpl {
  val problems = mmapEmpty[AbstractFile, MSet[Problem]]
  val problems1 = mmapEmpty[String, MSet[Problem1]]
  def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
    severity.count += 1
    problems.getOrElseUpdate(pos.source.file, msetEmpty) += Problem(pos, msg, severity.id)
  }
  def info1(title: String, msg: String, severity: Severity, force: Boolean): Unit = {
    severity.count += 1
    problems1.getOrElseUpdate(title, msetEmpty) += Problem1(title, msg, severity.id)
  }
}

object MsgLevel extends Enumeration {
  val INFO, WARNING, ERROR, NO = Value
}

class PrintReporter(msglevel: MsgLevel.Value) extends ReporterImpl {
  def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
    severity.count += 1
    severity match {
      case INFO    => if(msglevel <= MsgLevel.INFO) println(severity + "@" + pos + ":" + msg)
      case WARNING => if(msglevel <= MsgLevel.WARNING) System.err.println(severity + "@" + pos + ":" + msg)
      case ERROR   => if(msglevel <= MsgLevel.ERROR) System.err.println(severity + "@" + pos + ":" + msg)
    }
  }
  def info1(title: String, msg: String, severity: Severity, force: Boolean): Unit = {
    severity.count += 1
    severity match {
      case INFO    => if(msglevel <= MsgLevel.INFO) println(severity + "@" + title + ":" + msg)
      case WARNING => if(msglevel <= MsgLevel.WARNING) System.err.println(severity + "@" + title + ":" + msg)
      case ERROR   => if(msglevel <= MsgLevel.ERROR) System.err.println(severity + "@" + title + ":" + msg)
    }
  }
}

class NoReporter() extends ReporterImpl {
  
  def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
    
  }
  def info1(title: String, msg: String, severity: Severity, force: Boolean): Unit = {
    
  }
}

class FileReporter(outputUri: FileResourceUri, msglevel: MsgLevel.Value) extends ReporterImpl {
  val f = FileUtil.toFile(outputUri + "/.report")
  f.getParentFile.mkdirs
  f.delete()
  
  def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
    val fw = new FileWriter(f, true)
    val pw = new PrintWriter(new BufferedWriter(fw))
    severity.count += 1
    severity match {
      case INFO    => if(msglevel <= MsgLevel.INFO) pw.println(severity + "@" + pos + ":" + msg)
      case WARNING => if(msglevel <= MsgLevel.WARNING) pw.println(severity + "@" + pos + ":" + msg)
      case ERROR   => if(msglevel <= MsgLevel.ERROR) pw.println(severity + "@" + pos + ":" + msg)
    }
    pw.flush
    fw.close
    pw.close
  }
  def info1(title: String, msg: String, severity: Severity, force: Boolean): Unit = {
    val fw = new FileWriter(f, true)
    val pw = new PrintWriter(new BufferedWriter(fw))
    severity.count += 1
    severity match {
      case INFO    => if(msglevel <= MsgLevel.INFO) pw.println(severity + "@" + title + ":" + msg)
      case WARNING => if(msglevel <= MsgLevel.WARNING) pw.println(severity + "@" + title + ":" + msg)
      case ERROR   => if(msglevel <= MsgLevel.ERROR) pw.println(severity + "@" + title + ":" + msg)
    }
    pw.flush
    fw.close
    pw.close
  }
}
