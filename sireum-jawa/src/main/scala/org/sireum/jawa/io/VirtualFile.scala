/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa.io

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import scala.collection.mutable

/** This class implements an in-memory file.
 *
 *  @author  Philippe Altherr
 *  @version 1.0, 23/03/2004
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
class VirtualFile(val name: String, override val path: String) extends AbstractFile {
  /**
   * Initializes this instance with the specified name and an
   * identical path.
   *
   * @param name the name of the virtual file to be created
   * @return     the created virtual file
   */
  def this(name: String) = this(name, name)

  override def hashCode = path.hashCode
  override def equals(that: Any) = that match {
    case x: VirtualFile => x.path == path
    case _              => false
  }

  private var content = Array.emptyByteArray

  def absolute = this

  /** Returns null. */
  def file: JFile = null

  override def sizeOption: Option[Int] = Some(content.length)

  def input : InputStream = new ByteArrayInputStream(content)

  override def output: OutputStream = {
    new ByteArrayOutputStream() {
      override def close() {
        super.close()
        content = toByteArray()
      }
    }
  }

  def container: AbstractFile = NoAbstractFile

  /** Is this abstract file a directory? */
  def isDirectory: Boolean = false

  /** @inheritdoc */
  override def isVirtual: Boolean = true

  // private var _lastModified: Long = 0
  // _lastModified

  /** Returns the time that this abstract file was last modified. */
  // !!! Except it doesn't - it's private and never set - so I replaced it
  // with constant 0 to save the field.
  def lastModified: Long = 0

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile] = {
    assert(isDirectory, "not a directory '" + this + "'")
    Iterator.empty
  }

  /** Does this abstract file denote an existing file? */
  def create(): Unit = unsupported()

  /** Delete the underlying file or directory (recursively). */
  def delete(): Unit = unsupported()

  /**
   * Returns the abstract file in this abstract directory with the
   * specified name. If there is no such file, returns null. The
   * argument "directory" tells whether to look for a directory or
   * or a regular file.
   */
  def lookupName(name: String, directory: Boolean): AbstractFile = {
    assert(isDirectory, "not a directory '" + this + "'")
    null
  }

  /** Returns an abstract file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean) = unsupported()
}

/**
 * An in-memory directory.
 *
 * @author Lex Spoon
 *
 * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
class VirtualDirectory(val name: String, maybeContainer: Option[VirtualDirectory])
extends AbstractFile {
  def path: String =
    maybeContainer match {
      case None => name
      case Some(parent) => parent.path+'/'+ name
    }

  def absolute = this

  def container = maybeContainer.get
  def isDirectory = true
  override def isVirtual = true
  val lastModified: Long = System.currentTimeMillis

  override def file = null
  override def input = sys.error("directories cannot be read")
  override def output = sys.error("directories cannot be written")

  /** Does this abstract file denote an existing file? */
  def create() { unsupported() }

  /** Delete the underlying file or directory (recursively). */
  def delete() { unsupported() }

  /** Returns an abstract file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = unsupported()

  private val files = mutable.Map.empty[String, AbstractFile]

  // the toList is so that the directory may continue to be
  // modified while its elements are iterated
  def iterator = files.values.toList.iterator

  override def lookupName(name: String, directory: Boolean): AbstractFile =
    (files get name filter (_.isDirectory == directory)).orNull

  override def fileNamed(name: String): AbstractFile =
    Option(lookupName(name, directory = false)) getOrElse {
      val newFile = new VirtualFile(name, path+'/'+name)
      files(name) = newFile
      newFile
    }

  override def subdirectoryNamed(name: String): AbstractFile =
    Option(lookupName(name, directory = true)) getOrElse {
      val dir = new VirtualDirectory(name, Some(this))
      files(name) = dir
      dir
    }

  def clear() {
    files.clear()
  }
}
