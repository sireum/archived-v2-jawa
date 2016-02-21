/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.jawa.util

class MyTimer(val second : Int) {
  private var starttime : Long = 0
  def start = {
    starttime = System.currentTimeMillis()
  }
  def isTimeout : Boolean = {
    val duration = (System.currentTimeMillis() - starttime)/1000
    duration > second
  }
  def ifTimeoutThrow = {
    if(isTimeout)
      throw MyTimeoutException("Task running exceed " + second + "s!")
  }
}

class PerComponentTimer(second: Int) extends MyTimer(second)

case class MyTimeoutException(message : String) extends Exception
