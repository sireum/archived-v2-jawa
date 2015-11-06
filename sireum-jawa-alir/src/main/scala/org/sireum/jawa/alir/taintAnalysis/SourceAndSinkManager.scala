/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.taintAnalysis

import org.sireum.jawa.JawaMethod
import org.sireum.pilar.ast.JumpLocation
import org.sireum.pilar.ast.LocationDecl
import org.sireum.jawa.alir.pta.PTAResult
import org.sireum.util._
import java.io.BufferedReader
import java.io.FileReader
import java.util.regex.Pattern
import java.util.regex.Matcher
import org.sireum.jawa.Global
import org.sireum.jawa.alir.controlFlowGraph.ICFGNode
import org.sireum.jawa.alir.interProcedural.InterProceduralNode

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait SourceAndSinkManager {
  /**
   * it's a map from source API sig to it's category
   */
  protected val sources: MMap[String, ISet[String]] = mmapEmpty
  /**
   * it's a map from sink API sig to it's category
   */
  protected val sinks: MMap[String, (ISet[Int], ISet[String])] = mmapEmpty

  def parse(sasFilePath: String) =
    SSParser.parse(sasFilePath) match {
      case (sources, sinks) => 
        sources.foreach{
          case (sig, tags) =>
            this.sources += (sig -> tags)
        }
        sinks.foreach{
          case (sig, (poss, tags)) =>
            this.sinks += (sig -> (poss, tags))
        }
//        System.err.println("source size: " + this.sources.size + " sink size: " + this.sinks.size)
    }
  
  
  def addSource(source: String, tags: ISet[String]) = {
    this.sources += (source -> tags)
  }

  def addSink(sink: String, positions: ISet[Int], tags: ISet[String]) = {
    this.sinks += (sink -> (positions, tags))
  }
  
  def getSourceAndSinkNode[N <: InterProceduralNode](node: N, ptaresult: PTAResult): (ISet[TaintSource[N]], ISet[TaintSink[N]])
  def isSource(loc : LocationDecl, ptaresult : PTAResult) : Boolean
  def isSource(calleeMethod : JawaMethod, callerMethod : JawaMethod, callerLoc : JumpLocation) : Boolean
  def isSourceMethod(procedure : JawaMethod) : Boolean
  def isSink(loc : LocationDecl, ptaresult : PTAResult) : Boolean
  def isSink(procedure : JawaMethod) : Boolean
}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object SSParser{
  final val TITLE = "SSParser"
  final val DEBUG = false
  //                           1            2                   3            4
  private val regex = "([^\\s]+)\\s+([^\\s]+)?\\s*->\\s+([^\\s]+)\\s*([^\\s]+)?\\s*"
  def parse(filePath: String): (IMap[String, ISet[String]], IMap[String, (ISet[Int], ISet[String])]) = {
    def readFile: BufferedReader = new BufferedReader(new FileReader(filePath))
    val sources: MMap[String, ISet[String]] = mmapEmpty
    val sinks: MMap[String, (ISet[Int], ISet[String])] = mmapEmpty
    val p: Pattern = Pattern.compile(regex)
    val rdr = readFile
    var line = rdr.readLine()
    while(line != null){
      try{
        val m = p.matcher(line)
        if(m.find()){
          val (tag, apiSig, positions, tainttags) = parseLine(m)
          tag match{
            case "_SOURCE_" => sources += (apiSig -> tainttags)
            case "_SINK_" => sinks += (apiSig -> (positions, tainttags))
            case "_NONE_" =>
            case _ => throw new RuntimeException("Not expected tag: " + tag)
          }
        } else {
          throw new RuntimeException("Did not match the regex: " + line)
        }
      } catch {
        case ex: Exception =>
          if(DEBUG) ex.printStackTrace()
          System.err.println(TITLE + " exception occurs: " + ex.getMessage)
      }
      line = rdr.readLine()
    }
    (sources.toMap, sinks.toMap)
  }
  
  def parseLine(m: Matcher): (String, String, ISet[Int], ISet[String]) = {
    require(m.group(1) != null && m.group(3) != null)
    val apiSig = m.group(1)
    val taintTag = m.group(2)
    val taintTags: ISet[String] = if(taintTag == null) isetEmpty else taintTag.split("\\|").toSet
    val tag = m.group(3)
    val rawpos = m.group(4)
    val positions: MSet[Int] = msetEmpty
    if(rawpos != null) {
      positions ++= rawpos.split("\\|").map(_.toInt)
    }
    (tag, apiSig, positions.toSet, taintTags)
  }
}