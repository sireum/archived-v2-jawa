package org.sireum.jawa.sjc.interactive

import org.sireum.util._
import org.sireum.jawa.sjc.parser.CompilationUnit
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.SynchronizedMap
import org.sireum.jawa.io.SourceFile
import org.sireum.jawa.io.AbstractFile
import scala.collection.mutable.SynchronizedSet
import scala.collection.mutable.HashSet
import org.sireum.jawa.Problem

trait RichCompilationUnits { self: Global =>
  private val unitOfFile = new LinkedHashMap[AbstractFile, RichCompilationUnit] with
                       SynchronizedMap[AbstractFile, RichCompilationUnit] {
    override def put(key: AbstractFile, value: RichCompilationUnit) = {
      val r = super.put(key, value)
      r
    }
    override def remove(key: AbstractFile) = {
      val r = super.remove(key)
      r
    }
  }
  
  /** A set containing all those files that need to be removed
   *  Units are removed by getUnit, typically once a unit is finished compiled.
   */
  protected val toBeRemoved: MSet[AbstractFile] =
    new HashSet[AbstractFile] with SynchronizedSet[AbstractFile]

  /** A set containing all those files that need to be removed after a full background compiler run
   */
  protected val toBeRemovedAfterRun: MSet[AbstractFile] =
    new HashSet[AbstractFile] with SynchronizedSet[AbstractFile]
  
  def addCompilationUnit(file: AbstractFile, rcu: RichCompilationUnit) = this.unitOfFile.put(file, rcu)
  def addCompilationUnits(rcus: ISeq[RichCompilationUnit]) = {
    rcus.foreach(rcu => addCompilationUnit(rcu.cu.firstToken.file.file, rcu))
  }
  def removeCompilationUnit(file: AbstractFile) = this.unitOfFile.remove(file)
  def getCompilationUnits: IMap[AbstractFile, RichCompilationUnit] = this.unitOfFile.toMap
  def getCompilationUnit(file: AbstractFile): Option[RichCompilationUnit] ={
    toBeRemoved.synchronized{
      for(f <- toBeRemoved) {
        informIDE("removed: " + file)
        unitOfFile -= file
      }
      toBeRemoved.clear()
    }
    this.unitOfFile.get(file)
  }
  def hasCompilationUnit(file: AbstractFile): Boolean = this.unitOfFile.contains(file)
  def managedFiles: ISet[AbstractFile] = this.unitOfFile.keySet.toSet
  
  case class RichCompilationUnit(cu: CompilationUnit) {
    /** The problems reported for this unit */
    val problems: MList[Problem] = mlistEmpty
  }
}