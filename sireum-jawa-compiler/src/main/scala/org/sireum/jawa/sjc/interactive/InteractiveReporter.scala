/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.jawa.sjc.interactive

import org.sireum.jawa.ReporterImpl
import org.sireum.jawa.io.Position
import org.sireum.util._
import org.sireum.jawa.Problem

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
  
  override def info1(title: String, msg: String, severity: Severity, force: Boolean): Unit = {}

  override def reset() {
    super.reset()
    otherProblems.clear()
  }
}
