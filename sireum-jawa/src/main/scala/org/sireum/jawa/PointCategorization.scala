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

import org.sireum.util.ISet
import org.sireum.util.ResourceUri
import org.sireum.util.IMap

abstract class Point{
  def ownerSig: Signature
}

/**
 * Set of program points corresponding to l-value expressions. 
 * pl represents an element in this set.
 */
trait Left

/**
 * Set of program points corresponding to r-value expressions. 
 * This also include expressions which evaluate to void. 
 * pr represents an element in this set. Pr=P\Pl
 */
trait Right

/**
 * static variable
 */
trait Static_Field{def staticFieldFQN: FieldFQN}

/**
 * array
 */
trait Array{def dimensions: Int}

/**
 * object creation
 */
trait NewObj{def obj: JawaType}

/**
 * base variable
 */
trait Base{
  def baseName: String
  private var fieldP: Point with Field = null
  def setFieldPoint(f: Point with Field) = fieldP = f
  def getFieldPoint: Point with Field = fieldP
}

/**
 * field variable
 */
trait Field{
  def baseP: Point with Base
  def fqn: FieldFQN
}

/**
 * have location and index
 */
trait Loc{
  def loc: ResourceUri
  def locIndex: Int
}

trait Dynamic{
  def recvPCall: PointRecvCall
  def recvPReturn: PointRecvReturn
}

trait Virtual{
  def thisPEntry: PointThisEntry
  def thisPExit: PointThisExit
}

trait Invoke{
  def sig: Signature
  def invokeTyp: String
  def argPsCall: IMap[Int, PointArgCall]
  def argPsReturn: IMap[Int, PointArgReturn]
  def retTyp: JawaType
}

trait Method{
  def methodSig: Signature
  def accessTyp: String
  def paramPsEntry: IMap[Int, PointParamEntry]
  def paramPsExit: IMap[Int, PointParamExit]
  def retVar: Option[PointMethodRet]
}

trait Param{
  def paramName: String
  def index: Int
  def paramTyp: JawaType
}

trait Arg{
  def argName: String
  def index: Int
}

trait Entry

trait Exit

trait Call {
  private var container: Point with Invoke = null
  def setContainer(c: Point with Invoke) = container = c
  def getContainer: Point with Invoke = container
}

trait Return {
  private var container: Point with Invoke = null
  def setContainer(c: Point with Invoke) = container = c
  def getContainer: Point with Invoke = container
}
