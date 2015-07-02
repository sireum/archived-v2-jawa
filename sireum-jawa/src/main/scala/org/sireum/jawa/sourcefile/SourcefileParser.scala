package org.sireum.jawa.sourcefile

import org.sireum.jawa.io.SourceFile
import org.sireum.util.IMap
import org.sireum.jawa.ObjectType
import org.sireum.jawa.MyClass
import org.sireum.jawa.JawaResolver
import org.sireum.jawa.ResolveLevel
import org.sireum.jawa.LightWeightPilarParser


/**
 * @author fgwei
 */
object SourcefileParser {
  def parse(file: SourceFile, level: ResolveLevel.Value): IMap[ObjectType, MyClass] = {
    var code = file.code
    if(level < ResolveLevel.BODY) {
      code = LightWeightPilarParser.getEmptyBodyCode(code)
    }
    val st = JawaResolver.getSymbolResolveResult(Set(code))
    val v = new MySTVisitor
    v.resolveFromST(st, level, true)
    v.getClasses
  }
}