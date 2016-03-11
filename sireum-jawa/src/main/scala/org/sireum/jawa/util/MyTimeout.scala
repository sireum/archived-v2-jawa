package org.sireum.jawa.util

import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeoutException

case class MyTimeout(time: FiniteDuration) {
  private final var startTime: Long = System.currentTimeMillis()
  def refresh = this.startTime = System.currentTimeMillis()
  def isTimeout: Boolean = {
    val currentTime = System.currentTimeMillis()
    (currentTime - startTime) >= time.toMillis
  }
  def isTimeoutThrow = {
    if(isTimeout) throw new TimeoutException
  }
}