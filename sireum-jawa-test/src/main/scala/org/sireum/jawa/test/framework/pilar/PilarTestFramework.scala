/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.test.framework.pilar

import org.sireum.jawa.test.framework.TestFramework
import org.sireum.util.FileResourceUri
import java.io.File
import java.net.URI
import org.sireum.pilar.parser.Parser
import org.sireum.pilar.ast.PilarAstNode
import org.sireum.pilar.ast.Model
import org.sireum.jawa.symbolResolver.JawaSymbolTableBuilder
import org.sireum.jawa.symbolResolver.JawaSymbolTable
import org.sireum.jawa.JawaResolver
import org.sireum.jawa.alir.JawaAlirInfoProvider
import org.sireum.jawa.DefaultReporter
import org.sireum.jawa.Global
import org.sireum.jawa.io.NoPosition

class PilarTestFramework extends TestFramework {
  
  final val TITLE = "PilarTestFramework"
  
	def Analyzing : this.type = this

  def title(s : String) : this.type = {
    _title = caseString + s
    this
  }

  def file(fileRes : FileResourceUri) =
    InterProceduralConfiguration(title, fileRes)
/**
 * does inter procedural analysis of an app
 * @param src is the uri of the apk file
 */
  case class InterProceduralConfiguration //
  (title : String,
   srcRes : FileResourceUri) {

    test(title) {
      val global = new Global("test", new DefaultReporter)
      	global.reporter.echo(NoPosition, "####" + title + "#####")
      	
      	val pilarFileUri = srcRes
      	val reporter = new Parser.StringErrorReporter
  	    val modelOpt = Parser.parse[Model](Right(pilarFileUri), reporter, false)
  	    if(modelOpt.isDefined){
  	      global.reporter.echo(NoPosition, "Parsing OK!")
  	      val st = JawaSymbolTableBuilder.apply(List(modelOpt.get), { _ : Unit => new JawaSymbolTable }, false)
  	    } else {
  	      global.reporter.error(NoPosition, reporter.errorAsString)
  	    }
      	global.reporter.echo(NoPosition, "************************************\n")
    }
  }

  protected var _title : String = null
  protected var num = 0
  protected def title() = if (_title == null) {
    num += 1
    "Analysis #" + num
  } else _title
}