package org.sireum.jawa.sjc.util.refactoring

import org.sireum.jawa.sjc.io.AbstractFile
import org.sireum.jawa.sjc.parser.CompilationUnit

/**
 * @author fgwei
 */
trait CompilerAccess {

  val global: org.sireum.jawa.sjc.interactive.Global

  def compilationUnitOfFile(f: AbstractFile): Option[CompilationUnit]
}