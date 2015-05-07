package org.sireum.jawa.sjc.interactive

import org.sireum.util._
import org.sireum.jawa.sjc.ResolveLevel
import org.sireum.jawa.sjc.JavaKnowledge
import org.sireum.jawa.sjc.ObjectType

trait JawaClassLoadManager extends JavaKnowledge with JawaResolver with ResolveLevel { self: Global =>
  /**
   * didn't resolve this extends-relation list. It's a set of record names.
   */
  private val needToResolveExtends: MMap[JawaClass, MSet[ObjectType]] = mmapEmpty
  
  /**
   * didn't resolve this outer class name. 
   */
  private val needToResolveOuterClass: MMap[JawaClass, ObjectType] = mmapEmpty

  private val classesNeedUpdateInHierarchy: MSet[JawaClass] = msetEmpty
  
  /**
   * set of class names which not found in current environment
   */
  private val classNotFound: MSet[ObjectType] = msetEmpty
  
  /**
   * dirty flag to indicate whether the manager is clean
   */
  private var dirty: Boolean = false
  
  def isDirty: Boolean = this.dirty
  
  def addNeedToResolveOuterClass(innerclass: JawaClass, outerType: ObjectType) = {
    this.dirty = true
    needToResolveOuterClass(innerclass) = outerType
  }
  
  def addNeedToResolveExtend(childClass: JawaClass, parent: ObjectType) = {
    this.dirty = true
    needToResolveExtends.getOrElseUpdate(childClass, msetEmpty) += parent
  }
  
  def addNeedToResolveExtends(childClass: JawaClass, parents: ISet[ObjectType]) = {
    this.dirty = true
    needToResolveExtends.getOrElseUpdate(childClass, msetEmpty) ++= parents
  }
  
  def addClassesNeedUpdateInHierarchy(clazz: JawaClass) = this.classesNeedUpdateInHierarchy += clazz
  def getClassesNeedUpdateInHierarchy: ISet[JawaClass] = this.classesNeedUpdateInHierarchy.toSet
  def clearClassesNeedUpdateInHierarchy = this.classesNeedUpdateInHierarchy.clear()
  
  def addClassNotFound(typ: ObjectType) = this.classNotFound += typ
  def getClassNotFound: ISet[ObjectType] = this.classNotFound.toSet
  
  /**
   * resolve classes relation of the whole program
   */
  def resolveClassesRelationWholeProgram: Unit = {
    if(!isDirty) return
    val worklist: MList[JawaClass] = mlistEmpty
    val codes: MList[(Option[FileResourceUri], String)] = mlistEmpty
    worklist ++= needToResolveExtends.keySet ++ needToResolveOuterClass.keySet
    do{
      val tmpList: MList[JawaClass] = mlistEmpty
      while(!worklist.isEmpty){
        val clazz = worklist.remove(0)
        addClassesNeedUpdateInHierarchy(clazz)
        this.needToResolveOuterClass.get(clazz) match{
          case Some(o) =>
            tryGetClass(o) match{
              case Some(outer) =>
                this.needToResolveOuterClass -= clazz
                clazz.setOuterClass(outer)
                if(!this.needToResolveOuterClass.contains(outer) || this.needToResolveExtends.contains(outer)) worklist += outer
              case None =>
                if(JawaCodeSource.containsClass(o)){
                  val code = JawaCodeSource.getClassCode(o, ResolveLevel.HIERARCHY)
                  codes += ((Some(code._1), code._2))
                  tmpList += clazz
                } else {
                  this.needToResolveOuterClass -= clazz
                  val unknownOut = resolveClass(o, ResolveLevel.HIERARCHY)
                  addClassNotFound(o)
                  clazz.setOuterClass(unknownOut)
                }
            }
          case None =>
        }
        val resolved: MSet[ObjectType] = msetEmpty
        this.needToResolveExtends.getOrElse(clazz, msetEmpty).foreach{
          parType =>
            tryGetClass(parType) match{
              case Some(parent) =>
                resolved += parType
                if(parent.isInterface) clazz.addInterface(parent)
                else clazz.setSuperClass(parent)
                if(!this.needToResolveExtends.contains(parent) || this.needToResolveOuterClass.contains(parent)) worklist += parent
              case None =>
                if(JawaCodeSource.containsClass(parType)){
                  val code = JawaCodeSource.getClassCode(parType, ResolveLevel.HIERARCHY)
                  codes += ((Some(code._1), code._2))
                  tmpList += clazz
                } else {
                  resolved += parType
                  val unknownSu = resolveClass(parType, ResolveLevel.HIERARCHY)
                  addClassNotFound(parType)
                  clazz.setSuperClass(unknownSu)
                }
            }
        }
        val nre = this.needToResolveExtends(clazz)
        nre --= resolved
        if(nre.isEmpty) this.needToResolveExtends -= clazz
      }
      worklist ++= tmpList
      if(!codes.isEmpty) {
        val st = getCompilationUnitSymbolResult(this, codes.toList, ResolveLevel.HIERARCHY)
        resolveFromST(st, ResolveLevel.HIERARCHY, true)
      }
      codes.clear()
    } while(!codes.isEmpty)
      
    getClasses.foreach{
      rec =>
        if(!rec.hasSuperClass && rec.getName != JAVA_TOPLEVEL_OBJECT){
          if(!hasClass(JAVA_TOPLEVEL_OBJECT_TYPE)) resolveClass(JAVA_TOPLEVEL_OBJECT_TYPE, ResolveLevel.HIERARCHY)
          rec.setSuperClass(getClass(JAVA_TOPLEVEL_OBJECT_TYPE))
        }
    }
    this.dirty = !checkClassLoadingStatus
    if(isDirty) throw new RuntimeException("Class loading must have problem, since it should already finish.")
  }
  
  def checkClassLoadingStatus: Boolean = {
    this.needToResolveExtends.isEmpty && this.needToResolveOuterClass.isEmpty
  }
}