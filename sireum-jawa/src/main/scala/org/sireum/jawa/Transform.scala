package org.sireum.jawa

import org.sireum.util._
import org.sireum.jawa.symbolResolver.JawaSymbolTable
import org.sireum.pilar.ast._
import org.sireum.pilar.symbol._
import org.sireum.pilar.parser.ChunkingPilarParser
import org.sireum.jawa.symbolResolver.JawaSymbolTableBuilder


object Transform {
	var fst = { _ : Unit => new JawaSymbolTable }
  //for building symbol table
  var par : Boolean = false
  
	def parseCodes(codes : Set[String]) : List[Model] = {
	  codes.map{v => ChunkingPilarParser(Left(v), reporter) match{case Some(m) => m; case None => null}}.filter(v => v != null).toList
	}
	
	def reporter = {
	  new org.sireum.pilar.parser.PilarParser.ErrorReporter {
      def report(source : Option[FileResourceUri], line : Int,
                 column : Int, message : String) =
        System.err.println("source:" + source + ".line:" + line + ".column:" + column + "message" + message)
    }
	}
	
	def getSymbolResolveResult(codes : Set[String]) : SymbolTable = {
	  val newModels = parseCodes(codes)
	  JawaSymbolTableBuilder(newModels, fst, par)
	}
}