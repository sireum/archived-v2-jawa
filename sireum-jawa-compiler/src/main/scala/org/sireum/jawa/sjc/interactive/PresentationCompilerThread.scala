package org.sireum.jawa.sjc.interactive

import org.sireum.jawa.sjc.util.NoPosition

/** A presentation compiler thread. This is a lightweight class, delegating most
 *  of its functionality to the compiler instance.
 *
 */
final class PresentationCompilerThread(var compiler: Global, name: String = "")
  extends Thread("Scala Presentation Compiler [" + name + "]") {

  /** The presentation compiler loop.
   */
  override def run() {
    compiler.debugLog("starting new runner thread")
    while (compiler ne null) try {
      compiler.checkNoResponsesOutstanding()
      compiler.scheduler.waitForMoreWork()
      compiler.pollForWork(NoPosition)
      while (compiler.isOutOfDate) {
        try {
          compiler.backgroundCompile()
        } catch {
          case ex: FreshRunReq =>
            compiler.debugLog("fresh run req caught, starting new pass")
        }
      }
    } catch {
      case ex @ ShutdownReq =>
        compiler.debugLog("exiting presentation compiler")

        // make sure we don't keep around stale instances
        compiler = null
      case ex: Throwable =>
        ex match {
          case ex: FreshRunReq =>
            compiler.debugLog("fresh run req caught outside presentation compiler loop; ignored") // This shouldn't be reported
          case _ => ex.printStackTrace(); compiler.informIDE("Fatal Error: "+ex)
        }
    }
  }
}