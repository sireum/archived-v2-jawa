/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.taintAnalysis

import org.sireum.jawa.JawaProcedure
import org.sireum.pilar.ast.JumpLocation
import org.sireum.pilar.ast.LocationDecl
import org.sireum.util.ISet

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
trait SourceAndSinkManager[Fact] {
  def isSource(loc : LocationDecl, s : ISet[Fact]) : Boolean
  def isSource(calleeProcedure : JawaProcedure, callerProcedure : JawaProcedure, callerLoc : JumpLocation) : Boolean
  def isSourceProcedure(procedure : JawaProcedure) : Boolean
  def isSink(loc : LocationDecl, s : ISet[Fact]) : Boolean
  def isSinkProcedure(procedure : JawaProcedure) : Boolean
}