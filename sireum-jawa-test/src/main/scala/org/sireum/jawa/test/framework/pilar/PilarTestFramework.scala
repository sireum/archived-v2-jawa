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
import org.sireum.jawa.MessageCenter._
import java.io.File
import java.net.URI
import org.sireum.pilar.parser.Parser
import org.sireum.pilar.ast.PilarAstNode
import org.sireum.pilar.ast.Model
import org.sireum.jawa.Transform
import org.sireum.jawa.symbolResolver.JawaSymbolTableBuilder
import org.sireum.jawa.symbolResolver.JawaSymbolTable
import org.sireum.jawa.JawaResolver
import org.sireum.jawa.alir.JawaAlirInfoProvider
import org.sireum.jawa.Center

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
    	msg_critical(TITLE, "####" + title + "#####")
    	
    	val pilarFileUri = srcRes
    	val reporter = new Parser.StringErrorReporter
	    val modelOpt = Parser.parse[Model](Right(pilarFileUri), reporter, false)
	    if(modelOpt.isDefined){
	      msg_critical(TITLE, "Parsing OK!")
	      val st = JawaSymbolTableBuilder.apply(List(modelOpt.get), { _ : Unit => new JawaSymbolTable }, false)
	    } else {
	      err_msg_critical(TITLE, reporter.errorAsString)
	    }
    	msg_critical(TITLE, "************************************\n")
    }
  }

  protected var _title : String = null
  protected var num = 0
  protected def title() = if (_title == null) {
    num += 1
    "Analysis #" + num
  } else _title
}