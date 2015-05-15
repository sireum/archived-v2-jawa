package org.sireum.jawa.sjc.interactive

import org.sireum.util._
import org.sireum.jawa.sjc.parser.CompilationUnit
import org.sireum.jawa.sjc.io.AbstractFile

case class JawaDelta(changedOrDeletedCUFiles: ISet[AbstractFile], changedOrAddedCUs: ISeq[CompilationUnit])