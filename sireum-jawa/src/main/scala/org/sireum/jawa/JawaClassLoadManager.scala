package org.sireum.jawa

import org.sireum.util._
import org.sireum.jawa.io.NoPosition
import org.sireum.jawa.io.SourceFile
import org.sireum.jawa.io.AbstractFile

trait JawaClassLoadManager extends JavaKnowledge with JawaResolver { self: Global =>
  private final val TITLE = "JawaClassLoadManager"
  
  /**
   * set of classes contained by the current Global
   */
  protected val classes: MMap[ObjectType, JawaClass] = mmapEmpty
  
  /**
   * set of application classes contained by the current Global
   */
  protected val applicationClasses: MMap[ObjectType, JawaClass] = mmapEmpty
  
  /**
   * set of system library classes contained by the current Global
   */
  protected val systemLibraryClasses: MMap[ObjectType, JawaClass] = mmapEmpty
  
  /**
   * set of third party lib classes contained by the current Global
   */
  protected val userLibraryClasses: MMap[ObjectType, JawaClass] = mmapEmpty
  
  protected def getClassType(typ: ObjectType): ClassCategory.Value = {
    this.applicationClasses.contains(typ) match {
      case true => ClassCategory.APPLICATION
      case false =>
        this.userLibraryClasses.contains(typ) match {
          case true => ClassCategory.USER_LIBRARY
          case false =>
            ClassCategory.SYSTEM_LIBRARY
        }
    }
  }
  
  /**
   * set of entry points of the current Global
   */
//  protected val entryPoints: MSet[JawaMethod] = msetEmpty
  
  /**
   * class hierarchy of all classes in the current Global
   */
  protected val hierarchy: ClassHierarchy = new ClassHierarchy(reporter)

  /**
   * get all the application classes
   */
  def getApplicationClasses: ISet[JawaClass] = this.applicationClasses.values.toSet
  
  /**
   * get all the system library classes
   */
  def getSystemLibraryClasses: ISet[JawaClass] = this.systemLibraryClasses.values.toSet
  
  /**
   * get all the third party lib classes
   */
  def getUserLibraryClasses: ISet[JawaClass] = this.userLibraryClasses.values.toSet
  
  /**
   * get all the application classes
   */
  def isApplicationClasses(typ: ObjectType): Boolean = this.applicationClasses.contains(typ)
  
  /**
   * get all the system library classes
   */
  def isSystemLibraryClasses(typ: ObjectType): Boolean = this.systemLibraryClasses.contains(typ)
  
  /**
   * get all the third party lib classes
   */
  def isUserLibraryClasses(typ: ObjectType): Boolean = this.userLibraryClasses.contains(typ)
  
  /**
   * add an application class
   */
  def addApplicationClass(ar: JawaClass) = {
    if(this.applicationClasses.contains(ar.getType)) reporter.error(TITLE, "class " + ar.getName + " already exists in application class set.")
    else this.applicationClasses(ar.getType) = ar
  }
  
  /**
   * add a system library class
   */
  def addSystemLibraryClass(l: JawaClass) = {
    if(this.systemLibraryClasses.contains(l.getType)) reporter.error(TITLE, "class " + l.getName + " already exists in system library class set.")
    else this.systemLibraryClasses(l.getType) = l
  }
  
  /**
   * add a third party library class
   */
  def addUserLibraryClass(l: JawaClass) = {
    if(this.userLibraryClasses.contains(l.getType)) reporter.error(TITLE, "class " + l.getName + " already exists in user lib class set.")
    else this.userLibraryClasses(l.getType) = l
  }
  
  /**
   * has class
   */
  def hasClass(typ: ObjectType) = this.classes.contains(typ)
  
  /**
   * get classes
   */
  def getClasses: ISet[JawaClass] = this.classes.values.toSet
  
  /**
   * get class by type; if it does not exist, return None
   */
  def getClass(typ: ObjectType): Option[JawaClass] = {
    this.classes.get(typ)
  }
  
  /**
   * get class by type, if not present resolve it, if it still not exist, return None
   */
  def getClassOrResolve(typ: ObjectType): JawaClass = {
    getClass(typ) match {
      case None =>
        resolveToHierarchy(typ)
      case Some(a) => a
    }
  }
  
  def tryLoadClass(typ: ObjectType): Option[JawaClass] = {
    containsClassFile(typ) match {
      case true => Some(getClassOrResolve(typ))
      case false => None
    }
  }
  
  /**
   * remove application class
   */
  def removeApplicationClass(ar: JawaClass) = {
    if(!this.applicationClasses.contains(ar.getType)) reporter.error(TITLE, "class " + ar.getName + " does not exist in application class set.")
    else this.applicationClasses -= ar.getType
  }
  
  /**
   * remove System Library Class
   */
  def removeSystemLibraryClass(l: JawaClass) = {
    if(!this.systemLibraryClasses.contains(l.getType)) reporter.error(TITLE, "class " + l.getType + " does not exist in framework class set.")
    else this.systemLibraryClasses -= l.getType
  }
  
  /**
   * remove third party lib class
   */
  def removeUserLibraryClass(l: JawaClass) = {
    if(!this.userLibraryClasses.contains(l.getType)) reporter.error(TITLE, "class " + l.getType + " does not exist in user lib class set.")
    else this.userLibraryClasses(l.getType) = l
  }
  
  /**
   * get containing set of given class
   */
  def getContainingSet(ar: JawaClass): Set[JawaClass] = {
    if(ar.isApplicationClass) getApplicationClasses
    else if(ar.isSystemLibraryClass) getSystemLibraryClasses
    else if(ar.isUserLibraryClass) getUserLibraryClasses
    else null
  }
  
  /**
   * remove given class from containing set
   */
  def removeFromContainingSet(ar: JawaClass) = {
    if(ar.isApplicationClass) removeApplicationClass(ar)
    else if(ar.isSystemLibraryClass) removeSystemLibraryClass(ar)
    else if(ar.isUserLibraryClass) removeUserLibraryClass(ar)
  }
  
  /**
   * retrieve the normal class hierarchy
   */
  def getClassHierarchy: ClassHierarchy ={
    this.hierarchy.build(this)
    this.hierarchy
  }
  
  /**
   * reset class hierarchy
   */
  def resetClassHierarchy = this.hierarchy.reset
  
  /**
   * add class into Global
   */
  def addClass(ar: JawaClass) = {
    if(containsClass(ar) && getClass(ar.typ).get.getResolvingLevel >= ar.getResolvingLevel) 
      reporter.error(TITLE, "duplicate class: " + ar.getName)
    else {
      addClassInternal(ar)
      modifyHierarchy
    }
  }
  
  protected[jawa] def addClassInternal(ar: JawaClass) = {
    if(containsClass(ar) && getClass(ar.typ).get.getResolvingLevel >= ar.getResolvingLevel) 
      reporter.error(TITLE, "duplicate class: " + ar.getName)
    else {
      this.classes(ar.getType) = ar
      if(ar.isArray){
        ar.setSystemLibraryClass
      } else if (containsClassFile(ar.getType)){
        getClassCategoryFromClassPath(ar.getType) match {
          case ClassCategory.APPLICATION => ar.setApplicationClass
          case ClassCategory.USER_LIBRARY => ar.setUserLibraryClass
          case ClassCategory.SYSTEM_LIBRARY => ar.setSystemLibraryClass
        }
      } else {
        ar.setSystemLibraryClass
      }
    }
  }
  
  def removeClass(typ: ObjectType): Unit = {
    getClass(typ) foreach{removeClass(_)}
  }
  
  /**
   * remove class from Global
   */
  def removeClass(ar: JawaClass): Unit = {
    if(!containsClass(ar)) reporter.error(TITLE, "does not exist in Global: " + ar.getName)
    else {
      this.classes -= ar.getType
      if(ar.isSystemLibraryClass) this.systemLibraryClasses -= ar.getType
      else if(ar.isUserLibraryClass) this.userLibraryClasses -= ar.getType
      else if(ar.isApplicationClass) this.applicationClasses -= ar.getType
      modifyHierarchy
    }
  }
  
  protected def modifyHierarchy = {
    this.hierarchy.build(this)
  }
  
  /**
   * current Global contains the given class or not
   */
  def containsClass(ar: JawaClass) = this.classes.contains(ar.typ)
  
  /**
   * current Global contains the given class or not
   */
  def containsJawaClass(typ: ObjectType) = this.classes.contains(typ)
  
  /**
   * grab field from Global. Input example is java.lang.Throwable.stackState
   */
  def getField(fieldFQN: FieldFQN): Option[JawaField] = {
    try{
      val rType = fieldFQN.owner
      val fName = fieldFQN.fieldName
      if(!containsJawaClass(rType)) return None
      val r = getClass(rType).get
      if(!r.hasField(fName)) return None
      r.getField(fName)
    } catch {
      case t: Throwable => None
    }
  }
  
  /**
   * return true if contains the given field. Input example is java.lang.Throwable.stackState
   */
  def containsField(fieldFQN: FieldFQN): Boolean = getField(fieldFQN).isDefined
  
  /**
   * get procedure from Global. Input example is Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z
   */
  def getMethod(signature: Signature): Option[JawaMethod] = {
    val rType = signature.getClassType
    val subSig = signature.getSubSignature
    if(!containsJawaClass(rType)) return None
    val r = getClass(rType).get
    r.getMethod(subSig)
  }
  
//  def getMethodDeclarations(signature: Signature): Set[JawaMethod] = {
//    val result: MSet[JawaMethod] = msetEmpty
//    val rType = signature.getClassType
//    val subSig = signature.getSubSignature
//    if(!containsClass(rType)) resolveClass(rType, ResolveLevel.HIERARCHY)
//    val r = getClass(rType).get
//    val worklist: MList[JawaClass] = mlistEmpty
//    worklist += r
//    while(!worklist.isEmpty){
//      val rec = worklist.remove(0)
//      rec.getMethod(subSig) match{
//        case Some(proc) => result += proc
//        case None =>
//          if(rec.hasSuperClass) rec.getSuperClass foreach(worklist += _)
//          worklist ++= rec.getInterfaces
//      }
//    }
//    result.toSet
//  }
  
  /**
   * return true if contains the given procedure. Input example is Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z
   */
  def containsMethod(signature: Signature): Boolean = getMethod(signature).isDefined
  
  /**
   * find field from Global. Input: stackState
   */
//  def findField(baseType: ObjectType, fieldName: String): Option[JawaField] = {
//    tryLoadClass(baseType, ResolveLevel.HIERARCHY)
//    if(!containsClass(baseType)) return None
//    val r = getClass(baseType).get
//    r.getField(fieldName)
//  }
  
  /**
   * find field from Global. Input: @@java.lang.Throwable.stackState
   */
//  def findStaticField(fieldFQN: String): Option[JawaField] = {
//    val rName = getClassNameFromFieldFQN(fieldFQN)
//    val rType = getTypeFromName(rName).asInstanceOf[ObjectType]
//    val fieldName = getFieldNameFromFieldFQN(fieldFQN)
//    val classOpt = tryLoadClass(rType, ResolveLevel.HIERARCHY)
//    if(!classOpt.isDefined) return None
//    val r = classOpt.get
//    r.getField(fieldName) match {
//      case Some(f) =>  
//        if(f.isStatic)
//          Some(f)
//        else None
//      case None => None
//    }
//    
//  }
  
  /**
   * get entry points
   */
  def getEntryPoints(entryMethodName: String): ISet[JawaMethod] = {
    findEntryPoints(entryMethodName)
  }
    
  /**
   * set entry points
   */
//  def setEntryPoints(entryPoints: Set[JawaMethod]) = this.entryPoints ++= entryPoints
  
  /**
   * find entry points from current app/test cases
   */
  def findEntryPoints(entryMethodName: String): ISet[JawaMethod] = {
    val ep: MSet[JawaMethod] = msetEmpty
    getApplicationClasses.foreach{
      appRec =>
        if(appRec.declaresMethodByName(entryMethodName))
          appRec.getDeclaredMethodByName(entryMethodName) foreach{ep += _}
    }
    ep.toSet
  }
  
  /**
   * has entry points
   */
//  def hasEntryPoints: Boolean = !this.entryPoints.isEmpty
  
  /**
   * try to resolve given class and load all of the required support based on your desired resolve level.
   */
//  def tryLoadClass(typ: ObjectType, desiredLevel: ResolveLevel.Value): Option[JawaClass] = {
//    this.synchronized{
//      tryResolveClass(typ, desiredLevel)
//    }
//  }
  
  /**
   * resolve given class and load all of the required support.
   */
//  def loadClassAndSupport(typ: ObjectType): JawaClass = {
//    this.synchronized{
//      resolveClass(typ, ResolveLevel.BODY)
//    }
//  }
  
  /**
   * resolve given class.
   */
//  override def resolveClass(typ: ObjectType, desiredLevel: ResolveLevel.Value): JawaClass = {
//    this.synchronized{
//      super.resolveClass(typ, desiredLevel)
//    }
//  }
  
  /**
   * resolve given class.
   */
//  override def resolveClassFromSource(file: SourceFile, desiredLevel: ResolveLevel.Value): ISet[JawaClass] = {
//    this.synchronized{
//      super.resolveClassFromSource(file, desiredLevel)
//    }
//  }
  
  /**
   * resolve given class.
   */
//  def resolveClassFromSource(typ: ObjectType, file: SourceFile, desiredLevel: ResolveLevel.Value): Option[JawaClass] = {
//    this.synchronized{
//      val classes = resolveClassFromSource(file, desiredLevel)
//      classes.find { x => x.typ == typ }
//    }
//  }
  
  /**
   * softly resolve given class.
   */
//  def softlyResolveClass(typ: ObjectType, desiredLevel: ResolveLevel.Value): Option[JawaClass] = {
//    this.synchronized{
//      if(containsClass(typ))
//        Some(resolveClass(typ, desiredLevel))
//      else None
//    }
//  }
  
  /**
   * force resolve given class to given level
   */
//  override def forceResolveClass(typ: ObjectType, desiredLevel: ResolveLevel.Value): JawaClass = {
//    this.synchronized{
//      super.forceResolveClass(typ, desiredLevel)
//    }
//  }
  
  def printDetails = {
    println("***************Global***************")
    println("applicationClasses: " + getApplicationClasses)
    println("userLibraryClasses: " + getUserLibraryClasses)
    println("systemLibraryClasses: " + getSystemLibraryClasses)
    println("noCategorizedClasses: " + (getClasses -- getSystemLibraryClasses -- getUserLibraryClasses -- getApplicationClasses))
    println("hierarchy: " + getClassHierarchy)
    if(false){
      getClasses.foreach{
        case r=>
          r.printDetail
          r.getDeclaredFields.foreach(_.printDetail)
          r.getDeclaredMethods.foreach(_.printDetail)
      }
      getClassHierarchy.printDetails
    }
    println("******************************")
  }
  
  
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
  protected[jawa] def resolveClassesRelationWholeProgram: Unit = {
    if(!isDirty) return
    val worklist: MList[JawaClass] = mlistEmpty
    worklist ++= needToResolveExtends.keySet ++ needToResolveOuterClass.keySet
    while(!worklist.isEmpty) {
      val clazz = worklist.remove(0)
      addClassesNeedUpdateInHierarchy(clazz)
      this.needToResolveOuterClass.get(clazz) match{
        case Some(o) =>
          getClass(o) match{
            case Some(outer) =>
              this.needToResolveOuterClass -= clazz
              clazz.setOuterClass(outer)
            case None =>
              getMyClass(o) match {
                case Some(mc) =>
                  this.needToResolveOuterClass -= clazz
                  val outer = resolveFromMyClass(mc)
                  clazz.setOuterClass(outer)
                case None =>
                  this.needToResolveOuterClass -= clazz
                  val unknownOut = resolveToHierarchy(o)
                  clazz.setOuterClass(unknownOut)
              }
          }
        case None =>
      }
      this.needToResolveExtends.getOrElse(clazz, msetEmpty).foreach{
        parType =>
          getClass(parType) match{
            case Some(parent) =>
              this.needToResolveExtends -= clazz
              if(parent.isInterface) clazz.addInterface(parent)
              else clazz.setSuperClass(parent)
            case None =>
              getMyClass(parType) match {
                case Some(mc) =>
                  this.needToResolveExtends -= clazz
                  val parent = resolveFromMyClass(mc)
                  if(parent.isInterface) clazz.addInterface(parent)
                  else clazz.setSuperClass(parent)
                case None =>
                  this.needToResolveExtends -= clazz
                  val unknownSu = resolveToHierarchy(parType)
                  clazz.setSuperClass(unknownSu)
              }
          }
      }
      this.needToResolveExtends -= clazz
      this.needToResolveOuterClass -= clazz
      worklist ++= needToResolveExtends.keySet ++ needToResolveOuterClass.keySet
    }
      
    getClasses.foreach{
      rec =>
        if(!rec.hasSuperClass && rec.getName != JAVA_TOPLEVEL_OBJECT){
          if(!hasClass(JAVA_TOPLEVEL_OBJECT_TYPE)) resolveToHierarchy(JAVA_TOPLEVEL_OBJECT_TYPE)
          rec.setSuperClass(getClass(JAVA_TOPLEVEL_OBJECT_TYPE).get)
        }
    }
    this.dirty = !checkClassLoadingStatus
    if(isDirty) throw new RuntimeException("Class loading must have problem, since it should already finish.")
  }
  
  def checkClassLoadingStatus: Boolean = {
    this.needToResolveExtends.isEmpty && this.needToResolveOuterClass.isEmpty
  }
}