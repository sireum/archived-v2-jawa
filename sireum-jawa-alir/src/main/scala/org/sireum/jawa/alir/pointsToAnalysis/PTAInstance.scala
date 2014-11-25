/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.pointsToAnalysis

import org.sireum.jawa.Type
import org.sireum.jawa.alir.Context
import org.sireum.jawa.alir.Instance

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
final case class PTAInstance(typ : Type, defSite : Context) extends Instance{
  override def clone(newDefSite : Context) : Instance = PTAInstance(typ, newDefSite)
  override def toString : String = "(" + this.typ + "@" + this.defSite + ")"
}