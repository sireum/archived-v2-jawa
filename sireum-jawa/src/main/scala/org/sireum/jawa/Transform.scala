/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa

import org.sireum.util._
import org.sireum.jawa.symbolResolver.JawaSymbolTable
import org.sireum.pilar.ast._
import org.sireum.pilar.symbol._
import org.sireum.pilar.parser.ChunkingPilarParser
import org.sireum.jawa.symbolResolver.JawaSymbolTableBuilder
import org.sireum.pilar.parser.Parser


object Transform {
	var fst = { _ : Unit => new JawaSymbolTable }
  
	def parseCodes(codes : Set[String]) : Model = {
	  val sb = new StringBuilder
	  codes.foreach{
	    code => sb.append(code + "\n")
	  }
//	  ChunkingPilarParser(Left(sb.toString), reporter) match{case Some(m) => m; case None => throw new RuntimeException(sb.toString)}
	  val (modelopt, err) = Parser.parseWithErrorAsString[Model](Left(sb.toString)) 
	  modelopt match{case Some(m) => m; case None => throw new RuntimeException(err + "\n" + sb.toString)}
	}
	
	def getSymbolResolveResult(codes : Set[String]) : SymbolTable = {
	  val newModel = parseCodes(codes)
	  JawaSymbolTableBuilder(List(newModel), fst, GlobalConfig.jawaResolverParallel)
	}
}