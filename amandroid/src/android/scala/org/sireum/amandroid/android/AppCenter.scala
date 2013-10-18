package org.sireum.amandroid.android

import org.sireum.util._
import org.sireum.amandroid.AmandroidRecord
import org.sireum.amandroid.android.parser.IntentFilterDataBase
import org.sireum.amandroid.android.appInfo.AppInfoCollector
import org.sireum.amandroid.interProcedural.callGraph._

/**
 * this is an object, which hold information of apps. e.g. components, intent-filter database, etc.
 */
object AppCenter {
	private var components : ISet[AmandroidRecord] = isetEmpty
	
	private var intentFdb : IntentFilterDataBase = new IntentFilterDataBase()
	
	def setComponent(comp : AmandroidRecord) = this.components += comp
	
	def setComponents(comps : ISet[AmandroidRecord]) = this.components ++= comps
	
	def getComponents = this.components
	
	def setIntentFilterDB(i : IntentFilterDataBase) = this.intentFdb = i
	
	def updateIntentFilterDB(i : IntentFilterDataBase) = this.intentFdb.updateIntentFmap(i)
	
	def getIntentFilterDB ={
	  if(this.intentFdb == null) throw new RuntimeException("intent-filter database is not exist.")
	  this.intentFdb
	}
	
	/**
	 * hold application information (current only used for android app)
	 */
	
	private var appInfoOpt : Option[AppInfoCollector] = None
	
	/**
	 * set application info
	 */
	  
	def setAppInfo(info : AppInfoCollector) = this.appInfoOpt = Some(info)
	
	/**
	 * get application info
	 */
	  
	def getAppInfo : AppInfoCollector = 
	  this.appInfoOpt match{
	    case Some(info) => info
	    case None => throw new RuntimeException("AppInfo is not exist.")
  	}
	
	
	/**
	 * call graph of all procedures (app only)
	 */
	
	private var appOnlyCallGraph : CallGraph[CGNode] = null
	
	/**
	 * call graph of all procedures (whole program)
	 */
	
	private var wholeProgramCallGraph : CallGraph[CGNode] = null
	
	/**
	 * set call graph for the current center
	 */
	  
	def setAppOnlyCallGraph(cg : CallGraph[CGNode]) = this.appOnlyCallGraph = cg
	
	/**
	 * get call graph of the current center
	 */
	
	def getAppOnlyCallGraph : CallGraph[CGNode] = {
    if(!hasAppOnlyCallGraph) setAppOnlyCallGraph(new CallGraphBuilder().buildAppOnly(this.appInfoOpt))
    this.appOnlyCallGraph
  }
  
  /**
   * return true if current center has call graph
   */
  
  def hasAppOnlyCallGraph : Boolean = this.appOnlyCallGraph != null
  
  /**
   * release call graph
   */
  
  def releaseAppOnlyCallGraph = this.appOnlyCallGraph = null
  
  /**
	 * set call graph for the current center
	 */
	  
	def setWholeProgramCallGraph(cg : CallGraph[CGNode]) = this.wholeProgramCallGraph = cg
	
	/**
	 * get call graph of the current center
	 */
	
	def getWholeProgramCallGraph : CallGraph[CGNode] = {
    if(!hasWholeProgramCallGraph) setWholeProgramCallGraph(new CallGraphBuilder().buildWholeProgram(appInfoOpt))
    this.wholeProgramCallGraph
  }
  
  /**
   * return true if the current center has call graph
   */
  
  def hasWholeProgramCallGraph : Boolean = this.wholeProgramCallGraph != null
  
  /**
   * release call graph
   */
  
  def releaseWholeProgramCallGraph = this.wholeProgramCallGraph = null
	
  def reset = {
    this.components = isetEmpty
    this.appOnlyCallGraph = null
	  this.wholeProgramCallGraph = null
	  this.appInfoOpt = None
  }
}