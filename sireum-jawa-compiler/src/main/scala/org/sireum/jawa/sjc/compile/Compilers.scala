/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa.sjc.compile

import org.sireum.util._
import java.io.File
import org.sireum.jawa.Reporter
import org.sireum.jawa.sjc.log.Logger

/**
 * @author fgwei
 */
trait Compilers[JawaCompiler] {
  def javac: JavaCompiler
  def jawac: JawaCompiler
}

trait JavaCompiler {
  /**
   * Compiles Java sources using the provided classpath, 
   * output directory, and additional options. Output should 
   * be sent to the provided reporter
   */
  def compile(sources: IList[File], options: IList[String], log: Logger)
}
