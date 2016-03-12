/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa.alir.dataFlowAnalysis

import org.sireum.util.ISet
import org.sireum.jawa.alir.controlFlowGraph.ICFGNode

/**
 * Provide an Interface to let the developer get data facts corresponding
 * to each statement.
 * 
 * @author Fengguo Wei
 */
trait InterProceduralDataFlowAnalysisResult[LatticeElement] {
  def entrySet : ICFGNode => ISet[LatticeElement]
}
