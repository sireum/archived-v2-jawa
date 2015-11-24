package org.sireum.jawa.classpath

import org.sireum.jawa.io.AbstractFile
import FileUtils.AbstractFileOps

/**
 * Provides factory methods for flat classpath. When creating classpath instances for a given path,
 * it uses proper type of classpath depending on a types of particular files containing sources or classes.
 */
class FlatClasspathFactory extends ClasspathFactory[FlatClasspath] {

  override def newClasspath(file: AbstractFile): FlatClasspath =
    if (file.isJarOrZip)
      ZipAndJarFlatClasspathFactory.create(file)
    else if (file.isDirectory)
      new DirectoryFlatClasspath(file.file)
    else
      sys.error(s"Unsupported classpath element: $file")

  override def sourcesInPath(path: String): List[FlatClasspath] =
    for {
      file <- expandPath(path, expandStar = false)
      dir <- Option(AbstractFile getDirectory file)
    } yield createSourcePath(dir)

  private def createSourcePath(file: AbstractFile): FlatClasspath =
    if (file.isJarOrZip)
      ZipAndJarFlatSourcepathFactory.create(file)
    else if (file.isDirectory)
      new DirectoryFlatSourcePath(file.file)
    else
      sys.error(s"Unsupported sourcepath element: $file")
}