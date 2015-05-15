/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.interactive

import org.sireum.util._
import org.sireum.jawa.sjc.ResolveLevel
import org.sireum.jawa.sjc.JawaType
import org.sireum.jawa.sjc.Signature
import org.sireum.jawa.sjc.ObjectType
import org.sireum.jawa.sjc.Reporter
import org.sireum.jawa.sjc.util.NoPosition
import org.sireum.jawa.sjc.util.Position
import scala.annotation.elidable
import org.sireum.jawa.sjc.util.SourceFile
import org.sireum.jawa.sjc.parser.JawaAstNode
import org.sireum.jawa.sjc.parser.CompilationUnit
import org.sireum.jawa.sjc.lexer.{Token => JawaToken}

/**
 * This is the interactive compiler of Jawa.
 * 
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class Global(val projectName: String, val reporter: Reporter) extends {
  /* Is the compiler initializing? Early def, so that the field is true during the
   *  execution of the super constructor.
   */
  protected var initializing = true
} with RichCompilationUnits
  with CompilerLifecycleManagement
  with CompilerControl
  with JawaClassLoadManager 
  with ResolveLevel {
  
  
  val debugIDE: Boolean = true
  val verboseIDE: Boolean = true
  private var curTime = System.nanoTime
  protected def timeStep = {
    val last = curTime
    curTime = System.nanoTime
    ", delay = " + (curTime - last) / 1000000 + "ms"
  }

  /** Print msg only when debugIDE is true. */
  @inline final def debugLog(msg: => String) =
    if (debugIDE) println("[%s] %s".format(projectName, msg))

  /** Inform with msg only when verboseIDE is true. */
  @inline final def informIDE(msg: => String) =
    if (verboseIDE) println("[%s][%s]".format(projectName, msg))
    
  private[interactive] val NoResponse: Response[_] = new Response[Any]

  /** The response that is currently pending, i.e. the compiler
   *  is working on providing an asnwer for it.
   */
  private[interactive] var pendingResponse: Response[_] = NoResponse
  
  /** Implements CompilerControl.askParsedEntered */
  private[interactive] def getParsedEntered(source: SourceFile, keepLoaded: Boolean, response: Response[CompilationUnit], onSameThread: Boolean = true) {
    getCompilationUnit(source.file) match {
      case Some(unit) =>
        getParsedEnteredNow(source, response)
      case None =>
        try {
          if (keepLoaded || isOutOfDate && onSameThread)
            getCompilationUnitSymbolResult(this, source.file, ResolveLevel.BODY)
        } finally {
          if (keepLoaded || !isOutOfDate || onSameThread)
            getParsedEnteredNow(source, response)
          else
            getParsedEnteredResponses(source) += response
        }
    }
  }

  /** Parses and enters given source file, storing parse tree in response */
  private[interactive] def getParsedEnteredNow(source: SourceFile, response: Response[CompilationUnit]) {
    respond(response) {
      onUnitOf(source) { unit =>
        unit.cu
      }
    }
  }
  
  /** Implements CompilerControl.askLinkPos */
  private[interactive] def getLinkPos(token: JawaToken, response: Response[Position]) {
    informIDE("getLinkPos "+token)
    respond(response) {
//      if (token.owner.isClass) {
//        withTempUnit(source){ u =>
//          findMirrorSymbol(sym, u).pos
//        }
//      } else {
        debugLog("link not in class "+token)
        NoPosition
//      }
    }
  }
  
  // ----------------- Implementations of client commands -----------------------

  def respond[T](result: Response[T])(op: => T): Unit =
    respondGradually(result)(Stream(op))

  def respondGradually[T](response: Response[T])(op: => Stream[T]): Unit = {
    val prevResponse = pendingResponse
    try {
      pendingResponse = response
      if (!response.isCancelled) {
        var results = op
        while (!response.isCancelled && results.nonEmpty) {
          val result = results.head
          results = results.tail
          if (results.isEmpty) {
            response set result
            debugLog("responded"+timeStep)
          } else response setProvisionally result
        }
      }
    } catch {
      case CancelException =>
        debugLog("cancelled")
      case ex: FreshRunReq =>
        if (debugIDE) {
          println("FreshRunReq thrown during response")
          ex.printStackTrace()
        }
        response raise ex
        throw ex

      case ex @ ShutdownReq =>
        if (debugIDE) {
          println("ShutdownReq thrown during response")
          ex.printStackTrace()
        }
        response raise ex
        throw ex

      case ex: Throwable =>
        if (debugIDE) {
          println("exception thrown during response: "+ex)
          ex.printStackTrace()
        }
        response raise ex
    } finally {
      pendingResponse = prevResponse
    }
  }

  private[interactive] def reloadSource(source: SourceFile) {
    removeCompilationUnit(source.file)
    getCompilationUnitSymbolResult(this, source.file, ResolveLevel.BODY)
  }

  /** Make sure a set of compilation units is loaded and parsed */
  private def reloadSources(sources: List[SourceFile]) {
    sources foreach reloadSource
    moveToFront(sources)
  }

  /** Make sure a set of compilation units is loaded and parsed */
  private[interactive] def reload(sources: List[SourceFile], response: Response[Unit]) {
    informIDE("reload: " + sources)
    respond(response)(reloadSources(sources))
    demandNewCompilerRun()
  }

  private[interactive] def filesDeleted(sources: List[SourceFile], response: Response[Unit]) {
    informIDE("files deleted: " + sources)
    val deletedFiles = sources.map(_.file).toSet
    deletedFiles foreach (removeCompilationUnit(_))
    respond(response)(())
    demandNewCompilerRun()
  }
	
}

object CancelException extends Exception