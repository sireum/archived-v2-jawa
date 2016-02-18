package org.sireum.jawa.util

import java.util.concurrent.Executors
import scala.concurrent._

class MyExecutionContext extends ExecutionContext {
  val threadPool = Executors.newFixedThreadPool(100);

  def execute(runnable: Runnable) {
      threadPool.submit(runnable)
  }

  def reportFailure(t: Throwable) {}
}