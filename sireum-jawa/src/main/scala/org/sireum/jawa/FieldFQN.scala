/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.jawa

/**
 * @author fgwei
 */
case class FieldFQN(owner: JawaType, fieldName: String, typ: JawaType) extends JavaKnowledge {
  def this(fqn: String, typ: JawaType) = this(JavaKnowledge.getClassTypeFromFieldFQN(fqn), JavaKnowledge.getFieldNameFromFieldFQN(fqn), typ)
  def fqn: String = (owner.jawaName + "." + fieldName).intern()
  override def toString: String = (owner.jawaName + "." + fieldName + ":" + typ.jawaName).intern()
}
