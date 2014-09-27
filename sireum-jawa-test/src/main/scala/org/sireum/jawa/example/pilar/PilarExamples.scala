/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.amandroid.example.interprocedural

import org.sireum.jawa.example.Examples
import org.sireum.jawa.util.MyFileUtil
import org.sireum.jawa.util.ResourceRetriever
import org.sireum.jawa.test.TestConfig



/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */
object PilarExamples extends Examples{
  val jawaTestDir = System.getenv(TestConfig.JAWA_TEST_DIR)
  
  override val PILAR_FILE_EXT = ".pilar"
  
  def modelFiles = exampleFiles(sourceDirUri(jawaTestDir + "/model/"), PILAR_FILE_EXT)
  
  protected def getFileRets(path : String, ext : String) = {
    val fileNames = MyFileUtil.getResourceListing(this.getClass(), path, ext)
    fileNames.map(
      name =>
    		ResourceRetriever(this.getClass(), path, name)
    )
  }
 
}