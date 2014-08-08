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