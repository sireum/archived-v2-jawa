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

import java.io.File
import java.net.{URL, URLClassLoader}
import org.sireum.jawa.Reporter
import org.sireum.util._
import org.sireum.jawa.sjc.codegen.JavaByteCodeGenerator
import org.sireum.jawa.sjc.parser.JawaParser
import org.sireum.jawa.sjc.lexer.JawaLexer
import org.sireum.jawa.DefaultReporter
import org.sireum.jawa.io.FgSourceFile
import org.sireum.jawa.sjc.log.Logger
import org.sireum.jawa.io.PlainFile

/**
 * @author fgwei
 */
final class JawaCompiler () {
  val reporter = new DefaultReporter
  private def parser(s: Either[String, FgSourceFile]) = new JawaParser(JawaLexer.tokenise(s, reporter).toArray, reporter)
  def compile(sources: IList[File], outputDirs: Seq[File], reporter: Reporter, log: Logger, progress: CompileProgress): Unit = {
    sources foreach{
      source =>
        require(source.getPath.endsWith("pilar") || source.getPath.endsWith("plr"), "Wrong file extension to compile " + source)
        val file = new FgSourceFile(new PlainFile(source))
        val cu = parser(Right(file)).compilationUnit(true)
        val css = new JavaByteCodeGenerator().generate(cu)
        css foreach {
          case (typ, bcs) =>
            outputDirs.foreach {
              output =>
                JavaByteCodeGenerator.writeClassFile(output.getAbsolutePath, typ.getPackage.get, typ.name.substring(typ.name.lastIndexOf(".") + 1), bcs)
            }
        }
    }
  }
}

object JawaCompiler
{
  import io.IO.{copy, createDirectory, zip, jars, unzip, withTemporaryDirectory}

  def compileSources(sourceJars: Iterable[File], targetJar: File, id: String, compiler: RawCompiler)
  {
    val isSource = (f: File) => isSourceName(f.getName)
    def keepIfSource(files: Set[File]): Set[File] = if(files.exists(isSource)) files else Set()

    withTemporaryDirectory { dir =>
      val extractedSources = (Set[File]() /: sourceJars) { (extracted, sourceJar)=> extracted ++ keepIfSource(unzip(sourceJar, dir)) }
      val (sourceFiles, resources) = extractedSources.partition(isSource)
      withTemporaryDirectory { outputDirectory =>
        val start = System.currentTimeMillis
        try {
          compiler(sourceFiles.toSeq, ilistEmpty, outputDirectory, "-nowarn" :: Nil)
        }
        catch { case e: CompileFailed => throw new CompileFailed(e.arguments, "Error compiling jawa component '" + id + "'") }
        import io.Path._
        copy(resources x rebase(dir, outputDirectory))
        zip((outputDirectory ***) x_! relativeTo(outputDirectory), targetJar)
      }
    }
  }
  private def isSourceName(name: String): Boolean = name.endsWith(".pilar") || name.endsWith(".java")
}

private[this] object IgnoreProgress extends CompileProgress {
  def startUnit(unitPath: String) {}
  def advance(current: Int, total: Int) = true
}
