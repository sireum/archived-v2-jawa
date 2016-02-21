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
package org.sireum.jawa.backend

import org.sireum.jawa.classpath.Classpath
import org.sireum.jawa.classpath.FlatClasspath
import org.sireum.jawa.io.AbstractFile

/** The platform dependent pieces of Global.
 */
trait Platform {
//  val symbolTable: symtab.SymbolTable
//  import symbolTable._

  /** The old, recursive implementation of compiler classpath. */
  def classPath: Classpath

  /** The new implementation of compiler classpath. */
  private[jawa] def flatClassPath: FlatClasspath

  /** Update classpath with a substitution that maps entries to entries */
  def updateClassPath(subst: Map[Classpath, Classpath])

//  /** Any platform-specific phases. */
//  def platformPhases: List[SubComponent]
//
//  /** Symbol for a method which compares two objects. */
//  def externalEquals: Symbol
//
//  /** The various ways a boxed primitive might materialize at runtime. */
//  def isMaybeBoxed(sym: Symbol): Boolean

  /**
   * Tells whether a class with both a binary and a source representation
   * (found in classpath and in sourcepath) should be re-compiled. Behaves
   * on the JVM similar to javac, i.e. if the source file is newer than the classfile,
   * a re-compile is triggered. On .NET by contrast classfiles always take precedence.
   */
  def needCompile(bin: AbstractFile, src: AbstractFile): Boolean
}
