package org.sireum.jawa.util

class MyTimer(second : Int) {
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

case class MyTimeoutException(message : String) extends Exception