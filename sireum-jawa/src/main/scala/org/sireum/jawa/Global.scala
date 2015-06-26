package org.sireum.jawa

/**
 * @author fgwei
 */
class Global(val projectName: String, val reporter: Reporter) extends {
  /* Is the compiler initializing? Early def, so that the field is true during the
   *  execution of the super constructor.
   */
  protected var initializing = true
} with JawaClassLoadManager {
  
}