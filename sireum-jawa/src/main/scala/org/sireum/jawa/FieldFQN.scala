package org.sireum.jawa

/**
 * @author fgwei
 */
case class FieldFQN(owner: ObjectType, fieldName: String) extends JavaKnowledge {
  def this(fqn: String) = this(JavaKnowledge.getClassTypeFromFieldFQN(fqn), JavaKnowledge.getFieldNameFromFieldFQN(fqn))
  override def toString: String = (owner.jawaName + "." + fieldName).intern()
}