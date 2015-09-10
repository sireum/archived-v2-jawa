package org.sireum.jawa.sourcefile

import org.sireum.pilar.symbol.SymbolTable
import org.sireum.pilar.symbol.SymbolTableProducer
import org.sireum.jawa.ResolveLevel
import scala.collection.GenMap
import org.sireum.util._
import org.sireum.pilar.ast._
import org.sireum.jawa.ObjectType
import org.sireum.jawa.JavaKnowledge
import org.sireum.jawa.util.ASTUtil
import org.sireum.jawa.AccessFlag
import org.sireum.jawa.InheritanceError
import org.sireum.jawa.MyClass
import org.sireum.jawa.JawaType
import org.sireum.jawa.MyField
import org.sireum.jawa.MyMethod
import org.sireum.jawa.MethodBody
import org.sireum.jawa.FieldFQN

/**
 * @author fgwei
 */
class MySTVisitor {
  private val classes: MMap[ObjectType, MyClass] = mmapEmpty
  def getClasses: IMap[ObjectType, MyClass] = classes.toMap
  
  /**
   * resolve all the classes, fields and procedures from symbol table producer which are provided from symbol table model
   */
  def resolveFromST(st: SymbolTable, level: ResolveLevel.Value, par: Boolean): Unit = {
    val stp = st.asInstanceOf[SymbolTableProducer]
    resolveClasses(stp, level, par)
    resolveGlobalVars(stp, level, par)
    resolveMethods(stp, level, par)
  }
  
  /**
   * collect class info from symbol table
   */
  def resolveClasses(stp: SymbolTableProducer, level: ResolveLevel.Value, par: Boolean) = {
    val col: GenMap[ResourceUri, RecordDecl] = if(par) stp.tables.recordTable.par else stp.tables.recordTable
    val classes = col.map{
      case (uri, rd) =>
        val typ: ObjectType = JavaKnowledge.getTypeFromName(rd.name.name).asInstanceOf[ObjectType]
        val accessFlag: Int = AccessFlag.getAccessFlags(ASTUtil.getAccessFlag(rd))
        var superType: Option[ObjectType] = None
        val interfaces: MList[ObjectType] = mlistEmpty
        rd.extendsClauses.foreach {
          ec =>
            val isInterface: Boolean = ASTUtil.getKind(ec) == "interface"
            if(isInterface){
              interfaces += JavaKnowledge.getTypeFromName(ec.name.name).asInstanceOf[ObjectType]
            } else {
              if(superType != None) throw InheritanceError(ec.name.name + " should be interface") 
              superType = Some(JavaKnowledge.getTypeFromName(ec.name.name).asInstanceOf[ObjectType])
            }
        }
        var outerType: Option[ObjectType] = None
        if(JavaKnowledge.isInnerClass(typ)) outerType = Some(JavaKnowledge.getOuterTypeFrom(typ))
        
        val myclass = MyClass(accessFlag, typ, superType, interfaces.toList, outerType)
        this.classes(typ) = myclass
        
        rd.attributes.foreach{
          field =>
            val fieldType: JawaType = ASTUtil.getTypeFromTypeSpec(field.typeSpec.get)
            val FQN: FieldFQN = new FieldFQN(field.name.name, fieldType)
            val accessFlag: Int = AccessFlag.getAccessFlags(ASTUtil.getAccessFlag(field))
            val f = MyField(accessFlag, FQN)
            myclass.addField(f)
        }
        myclass
    }.toSet
    classes.foreach{
      c =>
        c.addField(createClassField(c))
    }
  }
  
  private def createClassField(rec: MyClass): MyField = {
    MyField(AccessFlag.getAccessFlags("FINAL_STATIC"), FieldFQN(rec.typ, "class", new ObjectType("java.lang.Class")))
  }
  
  /**
   * collect global variables info from the symbol table
   */
  def resolveGlobalVars(stp: SymbolTableProducer, level: ResolveLevel.Value, par: Boolean) = {
    val col: GenMap[ResourceUri, GlobalVarDecl] = if(par) stp.tables.globalVarTable.par else stp.tables.globalVarTable
    col.map{
      case (uri, gvd) =>
        require(gvd.typeSpec.isDefined)
        val globalVarType: JawaType = ASTUtil.getTypeFromTypeSpec(gvd.typeSpec.get)
        val FQN = new FieldFQN(gvd.name.name.replaceAll("@@", ""), globalVarType) // e.g. @@java.lang.Enum.serialVersionUID
        val accessFlag = AccessFlag.getAccessFlags(ASTUtil.getAccessFlag(gvd))
        val f = MyField(accessFlag, FQN)
        val ownerType = FQN.owner
        val owner = this.classes(ownerType)
        owner.addField(f)
    }
  }
  
  /**
   * collect method info from symbol table
   */
  def resolveMethods(stp: SymbolTableProducer, level: ResolveLevel.Value, par: Boolean) = {
    val ms = resolveMethodOnly(stp, level)
    ms foreach {
      m =>
        val ownerType = m.signature.getClassType
        val c: MyClass = this.classes(ownerType)
        c.addMethod(m)
    }
  }
  
  def resolveMethodOnly(stp: SymbolTableProducer, level: ResolveLevel.Value): ISet[MyMethod] = {
    val col = stp.tables.procedureAbsTable
    col.map{
      case (uri, pd) =>
        val signature = ASTUtil.getSignature(pd).get
        val accessFlag = AccessFlag.getAccessFlags(ASTUtil.getAccessFlag(pd))
        val paramNames = pd.params.map{_.name.name}.toList
        val m: MyMethod = MyMethod(accessFlag, signature, paramNames)
        
        if(level >= ResolveLevel.BODY){
            m.setBody(stp.procedureSymbolTableProducer(uri).asInstanceOf[MethodBody])
//          if(pd.body.isInstanceOf[ImplementedBody]){
//            val body = pd.body.asInstanceOf[ImplementedBody]
//            val catchclauses = body.catchClauses
//            catchclauses.foreach{
//              catchclause =>
//                require(catchclause.typeSpec.isDefined)
//                require(catchclause.typeSpec.get.isInstanceOf[NamedTypeSpec])
//                val excName = catchclause.typeSpec.get.asInstanceOf[NamedTypeSpec].name.name
//                proc.addExceptionHandler(excName, catchclause.fromTarget.name, catchclause.toTarget.name, catchclause.jump.target.name)
//            }
//          }
        }
        m
    }.toSet
  }
}