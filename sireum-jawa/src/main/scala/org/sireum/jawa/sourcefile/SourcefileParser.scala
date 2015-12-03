/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sourcefile

import org.sireum.jawa.io.SourceFile
import org.sireum.util.IMap
import org.sireum.jawa.MyClass
import org.sireum.jawa.JawaResolver
import org.sireum.jawa.ResolveLevel
import org.sireum.jawa.LightWeightPilarParser
import org.sireum.pilar.symbol.SymbolTable
import org.sireum.jawa.Reporter
import org.sireum.jawa.JawaType


/**
 * @author fgwei
 */
object SourcefileParser {
  final val TITLE = "SourcefileParser"
  def parse(file: SourceFile, level: ResolveLevel.Value, reporter: Reporter): IMap[JawaType, MyClass] = {
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