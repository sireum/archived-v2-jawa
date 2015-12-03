package org.sireum.jawa

/**
 * @author fgwei
 */
case class FieldFQN(owner: JawaType, fieldName: String, typ: JawaType) extends JavaKnowledge {
  def this(fqn: String, typ: JawaType) = this(JavaKnowledge.getClassTypeFromFieldFQN(fqn), JavaKnowledge.getFieldNameFromFieldFQN(fqn), typ)
  def fqn: String = (owner.jawaName + "." + fieldName).intern()
  override def toString: String = (owner.jawaName + "." + fieldName + ":" + typ.jawaName).intern()
}