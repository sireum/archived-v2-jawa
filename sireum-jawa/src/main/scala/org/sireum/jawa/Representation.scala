package org.sireum.jawa

import org.sireum.util._


case class MyClass(
            accessFlag: Int, 
            typ: ObjectType,
            superType: Option[ObjectType],
            interfaces: IList[ObjectType],
            var outerType: Option[ObjectType] = None,
            var fields: IList[MyField] = ilistEmpty,
            var methods: IList[MyMethod] = ilistEmpty) {
  protected[jawa] def setOuter(o: ObjectType) = this.outerType = Some(o)
  protected[jawa] def addField(f: MyField) = this.fields :+= f
  protected[jawa] def addMethod(m: MyMethod) = this.methods :+= m
}

case class MyField(accessFlag: Int, FQN: String, typ: JawaType) {
  
}

case class MyMethod(
            accessFlag: Int, 
            signature: Signature,
            var params: IList[String] = ilistEmpty,
            var body: Option[MethodBody] = None) {
  protected[jawa] def addParam(name: String) = this.params :+= name
  protected[jawa] def setBody(b: MethodBody) = this.body = Some(b)
}