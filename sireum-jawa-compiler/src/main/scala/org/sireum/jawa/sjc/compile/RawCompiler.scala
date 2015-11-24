package org.sireum.jawa.sjc.compile

import java.io.File

/** A basic interface to the compiler.  It is called in the same virtual machine, but no dependency analysis is done.  This
 * is used, for example, to compile the interface/plugin code..*/
class RawCompiler()
{
  def apply(sources: Seq[File], classpath: Seq[File], outputDirectory: File, options: Seq[String])
  {
    
  }
}
class CompileFailed(val arguments: Array[String], override val toString: String) extends FeedbackProvidedException