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

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object OsUtils {
  def main(args: Array[String]): Unit = {
    println(getOsName)
  }
  private var OS : String = null
  def getOsName : String = {
    if(OS == null) OS = System.getProperty("os.name")
    OS
  }
  def isWindows : Boolean = getOsName.startsWith("Windows")
  def isLinux : Boolean = getOsName.startsWith("Linux")
  def isMac : Boolean = getOsName.startsWith("Mac")
}
