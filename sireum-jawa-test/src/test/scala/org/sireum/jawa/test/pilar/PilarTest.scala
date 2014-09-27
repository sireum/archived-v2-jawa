/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.test.pilar

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.sireum.jawa.test.framework.pilar.PilarTestFramework
import org.sireum.amandroid.example.interprocedural.PilarExamples

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
@RunWith(classOf[JUnitRunner])
class PilarTest extends PilarTestFramework {
		PilarExamples.modelFiles.
//		filter { s => s.endsWith("model-name.pilar") }.
		foreach { fileUri =>
		  Analyzing title fileUri file fileUri
		}
}