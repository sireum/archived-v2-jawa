package org.sireum.jawa.sjc

import org.sireum.util._
import org.sireum.jawa.sjc.interactive.JawaMethod
import org.sireum.jawa.sjc.interactive.JawaClass

trait JavaKnowledge {
  def JAVA_TOPLEVEL_OBJECT: String = "java.lang.Object"
  def JAVA_TOPLEVEL_OBJECT_TYPE: ObjectType = ObjectType(JAVA_TOPLEVEL_OBJECT, 0)
  def JAVA_PRIMITIVES = Set("byte", "short", "int", "long", "float", "double", "boolean", "char", "void")
  /**
   * return whether given type is java primitive type
   */
  def isJavaPrimitive(typ: JawaType): Boolean = typ.isInstanceOf[PrimitiveType]
  
  /**
   * return whether given type is java primitive type
   */
  def isJavaPrimitive(name: String): Boolean = this.JAVA_PRIMITIVES.contains(name)
  
  def formatObjectTypeToObjectName(typ: ObjectType): String = {
    val d = typ.dimensions
    typ.typ match{
      case "byte" =>    assign("B", d, "[", true)
      case "char" =>    assign("C", d, "[", true)
      case "double" =>  assign("D", d, "[", true)
      case "float" =>   assign("F", d, "[", true)
      case "int" =>     assign("I", d, "[", true)
      case "long" =>    assign("J", d, "[", true)
      case "short" =>   assign("S", d, "[", true)
      case "boolean" => assign("Z", d, "[", true)
      case "void" =>    "V"
      case _ =>
        if(d <= 0) typ.typ
        else assign("L" + typ.typ + ";", d, "[", true)
    }
  }
  
  def formatTypeToTypeSignature(typ: JawaType): String = {
    val d = typ match{case ot: ObjectType => ot.dimensions; case _ => 0}
    typ.typ match{
      case "byte" =>    assign("B", d, "[", true)
      case "char" =>    assign("C", d, "[", true)
      case "double" =>  assign("D", d, "[", true)
      case "float" =>   assign("F", d, "[", true)
      case "int" =>     assign("I", d, "[", true)
      case "long" =>    assign("J", d, "[", true)
      case "short" =>   assign("S", d, "[", true)
      case "boolean" => assign("Z", d, "[", true)
      case "void" =>    "V"
      case _ =>
        assign("L" + typ.typ + ";", d, "[", true)
    }
  }
  
  def formatTypeToSignature(typ: JawaType): String = {
    val d = typ match{case ot: ObjectType => ot.dimensions; case _ => 0}
    typ.typ match{
      case "byte" =>    assign("B", d, "[", true)
      case "char" =>    assign("C", d, "[", true)
      case "double" =>  assign("D", d, "[", true)
      case "float" =>   assign("F", d, "[", true)
      case "int" =>     assign("I", d, "[", true)
      case "long" =>    assign("J", d, "[", true)
      case "short" =>   assign("S", d, "[", true)
      case "boolean" => assign("Z", d, "[", true)
      case "void" =>    "V"
      case _ =>
        assign("L" + typ.typ.replaceAll("\\.", "/") + ";", d, "[", true)
    }
  }
  
  /**
   * input: "[Ljava/lang/String;"  output: ("Ljava/lang/String;", 1)
   */
  private def getDimensionsAndRemoveArrayFromSig(sig: String): (String, Int) = {
    val d =
      if(sig.startsWith("["))
        sig.lastIndexOf('[') - sig.indexOf('[') + 1
      else 0
    val tmp = sig.substring(sig.lastIndexOf('[') + 1)
    (tmp, d)
  }
  
  /**
   * input ("java.lang.String", 1) output Type
   */
  protected def getType(typ: String, dimentions: Int): JawaType = {
    if(dimentions == 0 && isJavaPrimitive(typ)) PrimitiveType(typ)
    else ObjectType(typ, dimentions)
  }
  
  /**
   * input: "java.lang.String[]"  output: JawaType("java.lang.String", 1)
   */
  def getTypeFromName(name: String): JawaType = {
    var d: Int = 0
    var tmp = name
    while(tmp.endsWith("[]")){
      d += 1
      tmp = tmp.substring(0, tmp.length() - 2)
    }
    getType(tmp, d)
  }
  
  /**
   * convert type string from signature style to type style. [Ljava/lang/Object; -> (java.lang.Object, 1)
   */
  def formatSignatureToType(sig: String): JawaType = {
    val (tmp, d) = getDimensionsAndRemoveArrayFromSig(sig)
    tmp match{
      case "B" =>   getType("byte", d)
      case "C" =>   getType("char", d)
      case "D" =>   getType("double", d)
      case "F" =>   getType("float", d)
      case "I" =>   getType("int", d)
      case "J" =>   getType("long", d)
      case "S" =>   getType("short", d)
      case "Z" =>   getType("boolean", d)
      case "V" =>   getType("void", d)
      case _ =>
        getType(tmp.substring(1, tmp.length() - 1).replaceAll("\\/", ".").replaceAll("<\\*>", ""), d)
    }
  }
  
  /**
   * get outer class name from inner class name. e.g. android.os.Handler$Callback -> android.os.Handler
   */
  def getOuterTypeFrom(innerType: ObjectType): ObjectType = {
    if(!isInnerClass(innerType)) throw new RuntimeException("wrong innerType: " + innerType)
    new ObjectType(innerType.name.substring(0, innerType.name.lastIndexOf("$")))
  }
  
  /**
   * return true if the given typ is a inner class or not
   */
  def isInnerClass(typ: ObjectType): Boolean = !typ.isArray && typ.name.lastIndexOf("$") > 0
  
  /**
   * input ("Ljava/lang/String;", 1, "[", true) output "[Ljava/lang/String;"
   */
  protected def assign(str: String, dimension: Int, pattern: String, front: Boolean): String = {
    val sb = new StringBuffer
    if(front){
      for(d <- 1 to dimension) sb.append(pattern)
    }
    sb.append(str)
    if(!front){
      for(d <- 1 to dimension) sb.append(pattern)
    }
    sb.toString().intern()
  }
  
  /********************** JawaField related op **************************/
  
  /**
   * check if given string is field signature or not
   */
  def isFQN(str: String) = isValidFieldFQN(str)
  
  /**
   * generate signature of this field. input: ("java.lang.Throwable", "stackState") output: "java.lang.Throwable.stackState"
   */
  def generateFieldFQN(typ: ObjectType, name: String): String = {
    val sb = new StringBuffer
    sb.append(typ.name + "." + name)
    sb.toString().intern()
  }
  
  /**
   * FQN of the field. e.g. java.lang.Throwable.stackState or @@java:lang:Enum.sharedConstantsCache
   */
  def isValidFieldFQN(fqn: String): Boolean = fqn.lastIndexOf('.') > 0
  
  /**
   * FQN of the field. e.g. java.lang.Throwable.stackState or @@java:lang:Enum.sharedConstantsCache
   */
  def isValidFieldName(name: String): Boolean = !name.contains('.')
  
  /**
   * get field name from field FQN. e.g. java.lang.Throwable.stackState -> stackState
   */
  def getFieldNameFromFieldFQN(fqn: String): String = {
    if(!isValidFieldFQN(fqn)) throw new RuntimeException("given field signature is not a valid form: " + fqn)
    else fqn.substring(fqn.lastIndexOf('.') + 1)
  }
  
  /**
   * get class name from field signature. e.g. java.lang.Throwable.stackState -> java.lang.Throwable
   * [Ljava.lang.String;.length -> [Ljava.lang.String;
   */
  def getClassTypeFromFieldFQN(fqn: String): ObjectType = {
    val cn = getClassNameFromFieldFQN(fqn)
    getTypeFromName(cn).asInstanceOf[ObjectType]
  }
  
  /**
   * get class name from field signature. e.g. java.lang.Throwable.stackState -> java.lang.Throwable
   * [Ljava.lang.String;.length -> [Ljava.lang.String;
   */
  def getClassNameFromFieldFQN(fqn: String): String = {
    if(!isValidFieldFQN(fqn)) throw new RuntimeException("given field signature is not a valid form: " + fqn)
    fqn.substring(0, fqn.lastIndexOf('.'))
  }
  /********************** JawaField related op end **************************/
  
  /********************** JawaMethod related op **************************/
  
  /**
   * generate signature of this method
   */
  def generateSignature(method: JawaMethod): Signature = {
    val sb: StringBuffer = new StringBuffer
    val dc = method.getDeclaringClass
    sb.append(method.formatTypeToSignature(dc.getType))
    sb.append("." + generateSubSignature(method))
    Signature(sb.toString().intern())
  }
  
  /**
   * generate sub-signature of this method
   */
  private def generateSubSignature(method: JawaMethod): String = {
    val sb: StringBuffer = new StringBuffer
    val rt = method.getReturnType
    val pts = method.getParamTypes
    sb.append(method.getName + ":(")
    for(i <- 0 to pts.size - 1){
      val pt = pts(i) 
      sb.append(method.formatTypeToSignature(pt))
    }
    sb.append(")")
    sb.append(method.formatTypeToSignature(rt))
    sb.toString().intern()
  }
  
  def generateSignatureFromOwnerAndMethodSubSignature(clazz: JawaClass, subSig: String) : Signature = {
    val sig = clazz.formatTypeToSignature(clazz.getType) + "." + subSig
    Signature(sig)
  }
  
  def generateUnknownJawaMethod(declaringClass: JawaClass, signature: Signature): JawaMethod = {
    val name = signature.methodNamePart
    val thisOpt: Option[String] = Some("unknownThis")
    val paramTypes: IList[JawaType] = signature.getParameterTypes()
    val params: ISeq[(String, JawaType)] = Array.tabulate(paramTypes.length){ i => ("unknownParam" + i, paramTypes(i)) }.toList
    val returnType: JawaType = signature.getReturnType()
    val accessFlags = AccessFlag.getAccessFlags("PUBLIC")
    val method = JawaMethod(declaringClass, name, thisOpt, params, returnType, accessFlags)
    method.setUnknown
    method
  }
  
  /**
   * e.g. java.lang.Throwable.run
   */
  def isValidMethodFullName(mfn: String): Boolean = mfn.lastIndexOf('.') > 0
  
  def getClassNameFromMethodFullName(mfn: String): String = {
    if(!isValidMethodFullName(mfn)) throw new RuntimeException("given method full name is not a valid form: " + mfn)
    else mfn.substring(mfn.lastIndexOf('.') + 1)
  }
  
  def getClassTypeFromMethodFullName(mfn: String): ObjectType = {
    val cn = getClassNameFromMethodFullName(mfn)
    getTypeFromName(cn).asInstanceOf[ObjectType]
  }
  
  def getMethodNameFromMethodFullName(mfn: String): String = {
    if(!isValidMethodFullName(mfn)) throw new RuntimeException("given method full name is not a valid form: " + mfn)
    else mfn.substring(mfn.lastIndexOf('.') + 1)
  }
  /********************** JawaMethod related op end **************************/
  
  def constructorName: String = "<init>"
  def staticInitializerName: String = "<clinit>"
  def isJawaConstructor(name: String) = name == constructorName || name == staticInitializerName
}

object JavaKnowledge extends JavaKnowledge