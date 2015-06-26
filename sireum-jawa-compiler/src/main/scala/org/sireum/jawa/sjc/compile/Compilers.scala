package org.sireum.jawa.sjc.compile

import org.sireum.util._
import java.io.File
import org.sireum.jawa.Reporter
import org.sireum.jawa.log.Logger

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