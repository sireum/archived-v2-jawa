package org.sireum.jawa.alir.interProcedural.apiReachability

import org.sireum.jawa.alir.interProcedural.InterProceduralGraph
import org.sireum.jawa.alir.interProcedural.InterProceduralNode
import org.sireum.jawa.alir.Context

class BackwardCallGraph[Node <: BCGNode](initialAPISig : String) extends InterProceduralGraph[Node] {
	
}

sealed abstract class BCGNode(context : Context) extends InterProceduralNode(context)

abstract class BCGVirtualNode(context : Context) extends BCGNode(context) {
  def getVirtualLabel : String
  override def toString : String = getVirtualLabel + "@" + context
}

final case class BCGEntryNode(context : Context) extends BCGVirtualNode(context){
  def getVirtualLabel : String = "Entry"
  
}

final case class BCGExitNode(context : Context) extends BCGVirtualNode(context){
  def getVirtualLabel : String = "Exit"
}

final case class BCGLocNode(context : Context) extends BCGNode(context) {
  def getLocUri : String = context.getLocUri
  protected val LOC_INDEX = "LocIndex"
  def setLocIndex(i : Int) = setProperty(LOC_INDEX, i)
  def getLocIndex : Int = getPropertyOrElse[Int](LOC_INDEX, throw new RuntimeException("did not have loc index"))
}