package org.sireum.jawa.sourcefile

import org.sireum.jawa.io.SourceFile
import org.sireum.util.IMap
import org.sireum.jawa.ObjectType
import org.sireum.jawa.MyClass
import org.sireum.jawa.JawaResolver
import org.sireum.jawa.ResolveLevel
import org.sireum.jawa.LightWeightPilarParser
import org.sireum.pilar.symbol.SymbolTable
import org.sireum.jawa.Reporter


/**
 * @author fgwei
 */
object SourcefileParser {
  final val TITLE = "SourcefileParser"
  def parse(file: SourceFile, level: ResolveLevel.Value, reporter: Reporter): IMap[ObjectType, MyClass] = {
    var code = file.code
    if(level < ResolveLevel.BODY) {
      code = LightWeightPilarParser.getEmptyBodyCode(code)
    }
    val v = new MySTVisitor
    try {
      val st: SymbolTable = JawaResolver.getSymbolResolveResult(Set(code))
      v.resolveFromST(st, level)
    } catch {
      case e: Exception =>
        reporter.error(TITLE, e.getMessage)
        if(true) reporter.error(TITLE, code)
    }
    v.getClasses
  }
}