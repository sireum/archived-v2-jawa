package org.sireum.jawa.sjc.compile

import org.sireum.util._
import java.io.File


/**
 * @author fgwei
 */
trait Output {
  
}

trait SingleOutput extends Output {

  /** The directory where class files should be generated.
    * Incremental compilation will manage the class files in this directory.
    * In particular, outdated class files will be deleted before compilation.
    * It is important that this directory is exclusively used for one set of sources. */
  def outputDirectory(): File
}

trait MultipleOutput extends Output {

  trait OutputGroup {
    /** The directory where source files are stored for this group.
      * Source directories should uniquely identify the group for a source file. */
    def sourceDirectory(): File

    /** The directory where class files should be generated.
      * Incremental compilation will manage the class files in this directory.
      * In particular, outdated class files will be deleted before compilation.
      * It is important that this directory is exclusively used for one set of sources. */
    def outputDirectory(): File
  }

  def outputGroups(): IList[OutputGroup]
}