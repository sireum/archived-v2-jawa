/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.interactive

import org.sireum.util._
import org.sireum.jawa.ResolveLevel
import org.sireum.jawa.Reporter
import org.sireum.jawa.io.NoPosition
import org.sireum.jawa.io.Position
import scala.annotation.elidable
import org.sireum.jawa.io.SourceFile
import org.sireum.jawa.sjc.parser.JawaAstNode
import org.sireum.jawa.sjc.parser.CompilationUnit
import org.sireum.jawa.sjc.lexer.{Token => JawaToken}
import org.sireum.jawa.sjc.parser.JawaSymbol
import org.sireum.jawa.JawaClassLoadManager

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
  with JawaResolver{
  
  def logError(msg: String, t: Throwable): Unit = ()
  def inform(msg: String): Unit = ()
  
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
          if (keepLoaded || isOutOfDate && onSameThread){
            respond(response){
              parseCode[CompilationUnit](source, true).get
            }
          }
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
  
  private def withTempUnits[T](sources: List[SourceFile])(f: (SourceFile => RichCompilationUnit) => T): T = {
    val unitOfSrc: SourceFile => RichCompilationUnit = src => getCompilationUnit(src.file).get
    sources filterNot (getUnitOf(_).isDefined) match {
      case Nil =>
        f(unitOfSrc)
      case unknown =>
        reloadSources(unknown)
        try {
          f(unitOfSrc)
        } finally
          afterRunRemoveUnitsOf(unknown)
    }
  }

  private def withTempUnit[T](source: SourceFile)(f: RichCompilationUnit => T): T =
    withTempUnits(List(source)){ srcToUnit =>
      f(srcToUnit(source))
    }
  
  /** Implements CompilerControl.askLinkPos */
  private[interactive] def getLinkPos(sym: JawaSymbol, response: Response[Position]) {
    informIDE("getLinkPos "+sym)
    respond(response) {
//      if (token.owner.isClass) {
//        withTempUnit(source){ u =>
//          findMirrorSymbol(sym, u).pos
//        }
//      } else {
        debugLog("link not in class "+sym)
        NoPosition
//      }
    }
  }
  
  /** Set sync var `response` to a fully attributed tree located at position `pos`  */
  private[interactive] def getTypeAt(pos: Position, response: Response[Option[JawaSymbol]]) {
    respond(response)(typeAt(pos))
  }
  
  /** Arrange for unit to be removed after run, to give a chance to typecheck the unit fully.
   *  If we do just removeUnit, some problems with default parameters can ensue.
   *  Calls to this method could probably be replaced by removeUnit once default parameters are handled more robustly.
   */
  private def afterRunRemoveUnitsOf(sources: List[SourceFile]) {
    toBeRemovedAfterRun ++= sources map (_.file)
  }
  
  private[interactive] def typeAt(pos: Position): Option[JawaSymbol] = getUnitOf(pos.source) match {
    case None =>
      reloadSources(List(pos.source))
      try typeAt(pos)
      finally afterRunRemoveUnitsOf(List(pos.source))
    case Some(unit) =>
      informIDE("typeAt " + pos)
      val ast = locateAst(pos)
      debugLog("at pos "+pos+" was found: "+ast.getClass+" "+ast.pos.show)
      if(ast.isInstanceOf[JawaSymbol]) Some(ast.asInstanceOf[JawaSymbol])
      else None
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
    toBeRemoved -= source.file
    toBeRemovedAfterRun -= source.file
    val cu = parseCode[CompilationUnit](source, true).get
    addCompilationUnit(source.file, RichCompilationUnit(cu))
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
