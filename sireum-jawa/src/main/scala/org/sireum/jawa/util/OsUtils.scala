/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.util

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