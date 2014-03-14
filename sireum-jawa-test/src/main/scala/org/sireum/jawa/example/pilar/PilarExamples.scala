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