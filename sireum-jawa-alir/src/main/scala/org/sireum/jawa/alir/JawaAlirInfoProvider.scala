/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir

import org.sireum.util._
import org.sireum.jawa.symbolResolver.JawaSymbolTable
import org.sireum.pilar.symbol.SymbolTable
import org.sireum.alir.DefRef
import org.sireum.pilar.symbol.ProcedureSymbolTable
import org.sireum.pilar.ast.CatchClause
import org.sireum.alir.ControlFlowGraph
import org.sireum.jawa.ExceptionCenter
import org.sireum.pilar.ast.NamedTypeSpec
import org.sireum.pilar.parser.ChunkingPilarParser
import org.sireum.pilar.ast.Model
import org.sireum.jawa.symbolResolver.JawaSymbolTableBuilder
import org.sireum.alir.AlirIntraProceduralGraph
import org.sireum.jawa.alir.reachingDefinitionAnalysis.JawaReachingDefinitionAnalysis
import org.sireum.jawa.JawaMethod
import org.sireum.jawa.JawaClass
import org.sireum.pilar.ast.NameExp
import org.sireum.jawa.alir.reachingDefinitionAnalysis.JawaDefRef
import org.sireum.jawa.alir.reachingDefinitionAnalysis.JawaVarAccesses
import org.sireum.jawa.alir.pta.ClassInstance
import org.sireum.jawa.JawaResolver
import org.sireum.jawa.Global
import org.sireum.jawa.ObjectType

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object JawaAlirInfoProvider {
  
  final val CFG = "cfg"
  final val RDA = "rda"
  final val RDA_WITH_CALL = "rda_with_call"
  
  //for building cfg
  type VirtualLabel = String
  
  var dr : (SymbolTable, Boolean) => DefRef = { (st, b) => new JawaDefRef(st, new JawaVarAccesses(st), b) }
  
  val iopp : ProcedureSymbolTable => (ResourceUri => Boolean, ResourceUri => Boolean) = { pst =>
    val params = pst.params.toSet[ResourceUri]
    ({ localUri => params.contains(localUri) },
      { s => falsePredicate1[ResourceUri](s) })
  }
  
  val saom : Boolean = true
  
  def init(dr : (SymbolTable, Boolean) => DefRef) = {
    this.dr = dr
  }
  
  protected[jawa] def getExceptionType(cc : CatchClause) : ObjectType = {
    require(cc.typeSpec.isDefined)
    require(cc.typeSpec.get.isInstanceOf[NamedTypeSpec])
    val name = cc.typeSpec.get.asInstanceOf[NamedTypeSpec].name.name
    new ObjectType(name)
  }
  
  //for building cfg
  def siff(global: Global) : ControlFlowGraph.ShouldIncludeFlowFunction =
    { (loc, catchclauses) => 
      	var result = isetEmpty[CatchClause]
      	val thrownExcs = ExceptionCenter.getExceptionsMayThrow(loc)
      	thrownExcs.foreach{
      	  thrownException =>
          val child = global.getClassOrResolve(thrownException)
      	    val ccOpt = 
	      	    catchclauses.find{
			          catchclause =>
			            val excType = getExceptionType(catchclause)
			            val exc = global.getClassOrResolve(excType)
                  exc.global.getClassHierarchy.isClassRecursivelySubClassOfIncluding(child, exc)
	      	    }
      	    ccOpt match{
      	      case Some(cc) => result += cc
      	      case None =>
      	    }
           result ++= catchclauses
      	}
      	
      	(result, false)
    }
  
  def reporter = {
	  new org.sireum.pilar.parser.PilarParser.ErrorReporter {
      def report(source : Option[FileResourceUri], line : Int,
                 column : Int, message : String) =
        System.err.println("source:" + source + ".line:" + line + ".column:" + column + ".message:" + message)
    }
	}
  
	def getIntraMethodResult(code : String, global: Global) : Map[ResourceUri, TransformIntraMethodResult] = {
	  val newModel = JawaResolver.parseCodes(Set(code))
	  doGetIntraMethodResult(newModel, global)
	}
	
	def getIntraMethodResult(codes : Set[String], global: Global) : Map[ResourceUri, TransformIntraMethodResult] = {
	  val newModel = JawaResolver.parseCodes(codes)
	  doGetIntraMethodResult(newModel, global)
	}
	
	private def doGetIntraMethodResult(model: Model, global: Global) : Map[ResourceUri, TransformIntraMethodResult] = {
	  val result = JawaSymbolTableBuilder(List(model), JawaResolver.fst, true)
	  result.procedureSymbolTables.map{
	    pst=>
	      val (pool, cfg) = buildCfg(pst, global)
	      val rda = buildRda(pst, cfg, callref = false)
	      val procSig = 
	        pst.procedure.getValueAnnotation("signature") match {
			      case Some(exp : NameExp) =>
			        exp.name.name
			      case _ => throw new RuntimeException("Can not find signature")
			    }
	      (procSig, new TransformIntraMethodResult(pst, cfg, rda))
	  }.toMap
	}
  
  private def buildCfg(pst : ProcedureSymbolTable, global: Global) = {
	  val ENTRY_NODE_LABEL = "Entry"
	  val EXIT_NODE_LABEL = "Exit"
	  val pool : AlirIntraProceduralGraph.NodePool = mmapEmpty
	  val result = ControlFlowGraph[VirtualLabel](pst, ENTRY_NODE_LABEL, EXIT_NODE_LABEL, pool, siff(global))
	  (pool, result)
	}
	
	private def buildRda (pst : ProcedureSymbolTable, cfg : ControlFlowGraph[VirtualLabel], initialFacts : ISet[JawaReachingDefinitionAnalysis.RDFact] = isetEmpty, callref: Boolean) = {
	  val iiopp = iopp(pst)
	  JawaReachingDefinitionAnalysis[VirtualLabel](pst,
	    cfg,
	    dr(pst.symbolTable, callref),
	    first2(iiopp),
	    saom,
	    initialFacts)
	}
	
	/**
   * get cfg of current procedure
   */
  def getCfg(p : JawaMethod): ControlFlowGraph[VirtualLabel] = {
    if(!(p ? CFG)){
      this.synchronized{
	      val cfg = buildCfg(p.getBody, p.declaringClass.global)._2
	      p.setProperty(CFG, cfg)
      }
    }
    p.getProperty(CFG)
  }
	
	/**
   * get rda result of current procedure
   */
  def getRda(p : JawaMethod, cfg : ControlFlowGraph[VirtualLabel]): JawaReachingDefinitionAnalysis.Result = {
    if(!(p ? RDA)){
      this.synchronized{
	      val rda = buildRda(p.getBody, cfg, callref = false)
	      p.setProperty(RDA, rda)
      }
    }
    p.getProperty(RDA)
  }

  /**
   * get rda result of current procedure
   */
  def getRdaWithCall(p : JawaMethod, cfg : ControlFlowGraph[VirtualLabel]): JawaReachingDefinitionAnalysis.Result = {
    if(!(p ? RDA_WITH_CALL)){
      this.synchronized{
        val rda = buildRda(p.getBody, cfg, callref = true)
        p.setProperty(RDA_WITH_CALL, rda)
      }
    }
    p.getProperty(RDA_WITH_CALL)
  }
}

case class TransformIntraMethodResult(pst : ProcedureSymbolTable, cfg : ControlFlowGraph[String], rda : JawaReachingDefinitionAnalysis.Result)