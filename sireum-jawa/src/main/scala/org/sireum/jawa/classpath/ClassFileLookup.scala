package org.sireum.jawa.classpath

import org.sireum.jawa.io.AbstractFile
import java.net.URL

/**
 * Simple interface that allows us to abstract over how class file lookup is performed
 * in different classpath representations.
 */
trait ClassFileLookup {
  def findClassFile(name: String): Option[AbstractFile]

  /**
   * It returns both classes from class file and source files (as our base ClassRepresentation).
   * So note that it's not so strictly related to findClassFile.
   */
  def findClass(name: String): Option[ClassRepresentation]

  /**
   * A sequence of URLs representing this classpath.
   */
  def asURLs: Seq[URL]

  /** The whole classpath in the form of one String.
    */
  def asClasspathString: String

  /** The whole sourcepath in the form of one String.
    */
  def asSourcePathString: String
}

/**
 * Represents classes which can be loaded with a ClassfileLoader and/or SourcefileLoader.
 */
trait ClassRepresentation {
  def binary: Option[AbstractFile]
  def source: Option[AbstractFile]

  def name: String
}

object ClassRepresentation {
  def unapply[T](classRep: ClassRepresentation): Option[(Option[AbstractFile], Option[AbstractFile])] =
    Some((classRep.binary, classRep.source))
}