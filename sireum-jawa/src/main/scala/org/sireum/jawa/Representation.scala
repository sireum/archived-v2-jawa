/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa

import org.sireum.util._


case class MyClass(
            accessFlag: Int, 
            typ: JawaType,
            superType: Option[JawaType],
            interfaces: IList[JawaType],
            var outerType: Option[JawaType] = None,
            var fields: IList[MyField] = ilistEmpty,
            var methods: IList[MyMethod] = ilistEmpty) {
  protected[jawa] def setOuter(o: JawaType) = this.outerType = Some(o)
  protected[jawa] def addField(f: MyField) = this.fields :+= f
  protected[jawa] def addMethod(m: MyMethod) = this.methods :+= m
}

case class MyField(accessFlag: Int, FQN: FieldFQN) {
  
}

case class MyMethod(
            accessFlag: Int, 
            signature: Signature,
            var params: IList[String] = ilistEmpty,
            var body: Option[MethodBody] = None) {
  protected[jawa] def addParam(name: String) = this.params :+= name
  protected[jawa] def setBody(b: MethodBody) = this.body = Some(b)
}
