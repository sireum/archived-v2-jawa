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
import org.sireum.jawa.alir.intraProcedural.reachingDefinitionAnalysis.AmandroidReachingDefinitionAnalysis
import org.sireum.jawa.GlobalConfig
import org.sireum.jawa.Center
import org.sireum.jawa.JawaProcedure
import org.sireum.jawa.JawaRecord
import org.sireum.jawa.util.StringFormConverter
import org.sireum.jawa.Transform
import org.sireum.pilar.ast.NameExp

object JawaAlirInfoProvider {
  
  final val CFG = "cfg"
  final val RDA = "rda"
  
  //for building cfg
  type VirtualLabel = String
  
  val ERROR_TAG_TYPE = MarkerType(
  "org.sireum.pilar.tag.error.symtab",
  None,
  "Pilar Symbol Resolution Error",
  MarkerTagSeverity.Error,
  MarkerTagPriority.Normal,
  ilist(MarkerTagKind.Problem, MarkerTagKind.Text))
  
  val WARNING_TAG_TYPE = MarkerType(
  "org.sireum.pilar.tag.error.symtab",
  None,
  "Pilar Symbol Resolution Warning",
  MarkerTagSeverity.Warning,
  MarkerTagPriority.Normal,
  ilist(MarkerTagKind.Problem, MarkerTagKind.Text))
  
  var dr : SymbolTable => DefRef = null
  
  val iopp : ProcedureSymbolTable => (ResourceUri => Boolean, ResourceUri => Boolean) = { pst =>
    val params = pst.params.toSet[ResourceUri]
    ({ localUri => params.contains(localUri) },
      { s => falsePredicate1[ResourceUri](s) })
  }
  
  val saom : Boolean = true
  
  def init(dr : SymbolTable => DefRef) = {
    this.dr = dr
  }
  
  def getExceptionName(cc : CatchClause) : String = {
    require(cc.typeSpec.isDefined)
    require(cc.typeSpec.get.isInstanceOf[NamedTypeSpec])
    cc.typeSpec.get.asInstanceOf[NamedTypeSpec].name.name
  }
  
  //for building cfg
  val siff : ControlFlowGraph.ShouldIncludeFlowFunction =
    { (loc, catchclauses) => 
      	var result = isetEmpty[CatchClause]
      	val thrownExcNames = ExceptionCenter.getExceptionsMayThrow(loc)
      	if(thrownExcNames.forall(_ != ExceptionCenter.ANY_EXCEPTION)){
	      	val thrownExceptions = thrownExcNames.map(Center.resolveRecord(_, Center.ResolveLevel.HIERARCHY))
	      	thrownExceptions.foreach{
	      	  thrownException=>
	      	    val ccOpt = 
		      	    catchclauses.find{
				          catchclause =>
				            val excName = if(getExceptionName(catchclause) == ExceptionCenter.ANY_EXCEPTION) Center.DEFAULT_TOPLEVEL_OBJECT else getExceptionName(catchclause)
				            val exc = Center.resolveRecord(excName, Center.ResolveLevel.HIERARCHY)
				          	thrownException == exc || thrownException.isChildOf(exc)
		      	    }
	      	    ccOpt match{
	      	      case Some(cc) => result += cc
	      	      case None =>
	      	    }
	      	}
      	} else {
      	  result ++= catchclauses
      	}
      	
      	(result, false)
    } 
  
  def parseCodes(codes : Set[String]) : List[Model] = {
	  codes.map{v => ChunkingPilarParser(Left(v), reporter) match{case Some(m) => m; case None => null}}.filter(v => v != null).toList
	}
  
  def reporter = {
	  new org.sireum.pilar.parser.PilarParser.ErrorReporter {
      def report(source : Option[FileResourceUri], line : Int,
                 column : Int, message : String) =
        System.err.println("source:" + source + ".line:" + line + ".column:" + column + "message" + message)
    }
	}
  
	def getIntraProcedureResult(code : String) : Map[ResourceUri, TransformIntraProcedureResult] = {
	  val newModels = parseCodes(Set(code))
	  doGetIntraProcedureResult(newModels)
	}
	
	def getIntraProcedureResult(codes : Set[String]) : Map[ResourceUri, TransformIntraProcedureResult] = {
	  val newModels = parseCodes(codes)
	  doGetIntraProcedureResult(newModels)
	}
	
	private def doGetIntraProcedureResult(models : List[Model]) : Map[ResourceUri, TransformIntraProcedureResult] = {
	  val result = JawaSymbolTableBuilder(models, Transform.fst, GlobalConfig.jawaResolverParallel)
	  result.procedureSymbolTables.map{
	    pst=>
	      val (pool, cfg) = buildCfg(pst)
	      val rda = buildRda(pst, cfg)
	      val procSig = 
	        pst.procedure.getValueAnnotation("signature") match {
			      case Some(exp : NameExp) =>
			        exp.name.name
			      case _ => throw new RuntimeException("Can not find signature")
			    }
	      (procSig, new TransformIntraProcedureResult(pst, cfg, rda))
	  }.toMap
	}
  
  private def buildCfg(pst : ProcedureSymbolTable) = {
	  val ENTRY_NODE_LABEL = "Entry"
	  val EXIT_NODE_LABEL = "Exit"
	  val pool : AlirIntraProceduralGraph.NodePool = mmapEmpty
	  val result = ControlFlowGraph[VirtualLabel](pst, ENTRY_NODE_LABEL, EXIT_NODE_LABEL, pool, siff)
	  (pool, result)
	}
	
	private def buildRda (pst : ProcedureSymbolTable, cfg : ControlFlowGraph[VirtualLabel], initialFacts : ISet[AmandroidReachingDefinitionAnalysis.RDFact] = isetEmpty) = {
	  val iiopp = iopp(pst)
	  AmandroidReachingDefinitionAnalysis[VirtualLabel](pst,
	    cfg,
	    dr(pst.symbolTable),
	    first2(iiopp),
	    saom,
	    initialFacts)
	}
	
	/**
   * get cfg of current procedure
   */
  
  def getCfg(p : JawaProcedure) = {
    if(!(p ? CFG)){
      this.synchronized{
	      val cfg = buildCfg(p.getProcedureBody)._2
	      p.setProperty(CFG, cfg)
      }
    }
    p.getProperty(CFG).asInstanceOf[ControlFlowGraph[VirtualLabel]]
  }
	
	/**
   * get rda result of current procedure
   */
  
  def getRda(p : JawaProcedure, cfg : ControlFlowGraph[VirtualLabel]) = {
    if(!(p ? RDA)){
      this.synchronized{
	      val rda = buildRda(p.getProcedureBody, cfg)
	      p.setProperty(RDA, rda)
      }
    }
    p.getProperty(RDA).asInstanceOf[AmandroidReachingDefinitionAnalysis.Result]
  }
  
  def getClassInstance(r : JawaRecord) : ClassInstance = {
    val mainContext = new Context(GlobalConfig.CG_CONTEXT_K)
    mainContext.setContext("Center", "L0000")
    val className = StringFormConverter.formatRecordNameToClassName(r.getName)
    ClassInstance(className, mainContext)
  }
}

case class TransformIntraProcedureResult(pst : ProcedureSymbolTable, cfg : ControlFlowGraph[String], rda : AmandroidReachingDefinitionAnalysis.Result)