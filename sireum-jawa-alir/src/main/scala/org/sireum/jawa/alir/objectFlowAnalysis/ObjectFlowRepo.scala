/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.objectFlowAnalysis

import org.sireum.util._

trait ObjectFlowRepo[ValueSet <: NormalValueSet] {
  
  /**
   * tracking global variables 
   */ 
  final val globalDefRepo : MMap[String, (MSet[OfaGlobalVarNode], ValueSet)] = mmapEmpty
  
  /**
   * tracking all array variables
   */ 
  final val arrayRepo : MMap[ResourceUri, Int] = mmapEmpty
  
  /**
   * tracking all field variables
   */ 
  final var fieldVarRepo : Set[ResourceUri] = Set()
}