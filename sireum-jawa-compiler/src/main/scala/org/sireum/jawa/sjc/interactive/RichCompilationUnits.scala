package org.sireum.jawa.sjc.interactive

import org.sireum.util._
import org.sireum.jawa.sjc.parser.CompilationUnit
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.SynchronizedMap
import org.sireum.jawa.sjc.util.SourceFile
import org.sireum.jawa.sjc.io.AbstractFile

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
  
  def addCompilationUnit(file: AbstractFile, rcu: RichCompilationUnit) = this.unitOfFile.put(file, rcu)
  def addCompilationUnits(rcus: ISeq[RichCompilationUnit]) = {
    rcus.foreach(rcu => addCompilationUnit(rcu.cu.firstToken.file.file, rcu))
  }
  def removeCompilationUnit(file: AbstractFile) = this.unitOfFile.remove(file)
  def getCompilationUnits: IMap[AbstractFile, RichCompilationUnit] = this.unitOfFile.toMap
  def getCompilationUnit(file: AbstractFile): Option[RichCompilationUnit] = this.unitOfFile.get(file)
  def hasCompilationUnit(file: AbstractFile): Boolean = this.unitOfFile.contains(file)
  def managedFiles: ISet[AbstractFile] = this.unitOfFile.keySet.toSet
  
  case class RichCompilationUnit(cu: CompilationUnit) {
    /** The problems reported for this unit */
    val problems: MList[Problem] = mlistEmpty
  }
}