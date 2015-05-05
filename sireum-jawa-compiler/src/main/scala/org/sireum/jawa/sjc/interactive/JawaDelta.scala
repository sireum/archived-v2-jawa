package org.sireum.jawa.sjc.interactive

import org.sireum.util._
import org.sireum.jawa.sjc.parser.CompilationUnit

case class JawaDelta(changedOrDeletedCUFiles: ISet[FileResourceUri], changedOrAddedCUs: ISeq[CompilationUnit])