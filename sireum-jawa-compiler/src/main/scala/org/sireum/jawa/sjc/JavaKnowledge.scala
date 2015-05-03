package org.sireum.jawa.sjc

trait JavaKnowledge {
  val JAVA_TOPLEVEL_OBJECT = "java.lang.Object"
  val JAVA_PRIMITIVES = Set("byte", "short", "int", "long", "float", "double", "boolean", "char")
  /**
   * return whether given type is java primitive type
   */
  def isJavaPrimitive(typ: Type): Boolean = !typ.isArray && this.JAVA_PRIMITIVES.contains(typ.typ)
  
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
        assign("L" + typ.typ + ";", d, "[", true)
    }
  }
  
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
  
  def constructorName: String = "<init>"
  def staticInitializerName: String = "<clinit>"
}