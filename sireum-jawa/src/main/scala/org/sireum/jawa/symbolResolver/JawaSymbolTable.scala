/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.symbolResolver

import org.sireum.pilar.symbol._
import org.sireum.util._
import org.sireum.jawa.MethodBody

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class JawaSymbolTable extends SymbolTable with SymbolTableProducer {
  st =>
  val tables = SymbolTableData()
  val tags = marrayEmpty[LocationTag]
  var hasErrors = false

  val ERROR_TAG_TYPE = MarkerType(
    "org.sireum.pilar.tag.error.symtab",
    None,
    "Pilar Symbol Resolution Error",
    MarkerTagSeverity.Error,
    MarkerTagPriority.Normal,
    ilist(MarkerTagKind.Problem, MarkerTagKind.Text))

  val WARNING_TAG_TYPE = MarkerType(
    "org.sireum.pilar.tag.error.symtab",
    None,
    "Pilar Symbol Resolution Warning",
    MarkerTagSeverity.Warning,
    MarkerTagPriority.Normal,
    ilist(MarkerTagKind.Problem, MarkerTagKind.Text))

  def reportError(source : Option[FileResourceUri], line : Int,
                    column : Int, message : String) : Unit = {
      tags += Tag.toTag(source, line, column, message, ERROR_TAG_TYPE)
      hasErrors = true
    }

  def reportWarning(fileUri : Option[String], line : Int,
                    column : Int, message : String) : Unit =
    tags += Tag.toTag(fileUri, line, column, message, WARNING_TAG_TYPE)

  def reportError(source : Option[FileResourceUri], line : Int,
                  column : Int, offset : Int, length : Int,
                  message : String) : Unit = {
    tags += Tag.toTag(source, line, column, offset, length, message,
      ERROR_TAG_TYPE)
    hasErrors = true
  }

  def reportWarning(fileUri : Option[String], line : Int,
                    column : Int, offset : Int, length : Int,
                    message : String) : Unit =
    tags += Tag.toTag(fileUri, line, column, offset, length, message,
      WARNING_TAG_TYPE)

  val pdMap = mmapEmpty[ResourceUri, MethodBody]

  def globalVars = tables.globalVarTable.keys
  def globalVar(globalUri : ResourceUri) = tables.globalVarTable(globalUri)

  def procedures = tables.procedureTable.keys

  def procedures(procedureUri : ResourceUri) = tables.procedureTable(procedureUri)

  def procedureSymbolTables = pdMap.values

  def procedureSymbolTable(procedureAbsUri : ResourceUri) : ProcedureSymbolTable =
    procedureSymbolTableProducer(procedureAbsUri)

  def procedureSymbolTableProducer(procedureAbsUri : ResourceUri) = {
    assert(tables.procedureAbsTable.contains(procedureAbsUri))
    pdMap.getOrElseUpdate(procedureAbsUri, new MethodBody(procedureAbsUri, st))
  }

  def toSymbolTable : SymbolTable = this
}