/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.objectFlowAnalysis

import org.sireum.jawa.alir.Context
import org.sireum.jawa._

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
case class ProcedurePointNode[Node](thisEntryNodeOpt : Option[Node], 
    																 thisExitNodeOpt : Option[Node], 
    																 paramEntryNodes : Map[Int, Node], 
    																 paramExitNodes : Map[Int, Node],
    																 retNodeOpt : Option[Node],
    																 procPoint : PointProc) {

}

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
case class InvokePointNode[Node](recvCallNodeOpt : Option[Node], 
  																recvReturnNodeOpt : Option[Node], 
  																argCallNodes : Map[Int, Node], 
  																argReturnNodes : Map[Int, Node], 
  																piNodeOpt : Option[Node], 
  																invokePoint : PointI) {
  private var context : Context = null
  def setContext(context : Context) = this.context = context
	def getContext = context
	private var calleeSig : String = null
	def setCalleeSig(calleeSig : String) = this.calleeSig = calleeSig
	def getCalleeSig = calleeSig
	def contains(node : Node) : Boolean = {
	  recvCallNodeOpt match{
	    case Some(rcN) => 
	      if(rcN == node) return true 
	    case None =>
	  }
	  recvReturnNodeOpt match{
	    case Some(rrN) => if(rrN == node) return true 
	    case None =>
	  }
	  
	  argCallNodes.foreach{case(i, acN) => 
	    if(acN == node) return true }
	  argReturnNodes.foreach{case(i, arN) => if(arN == node) return true }
	  if(piNodeOpt == Some(node)) return true
	  false
	}
}