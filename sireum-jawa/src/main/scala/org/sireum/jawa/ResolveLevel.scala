package org.sireum.jawa

trait ResolveLevel {
	/**
   * resolving level of current record
   */
  
  protected var resolvingLevel : Center.ResolveLevel.Value = null
  
  /**
   * check whether we already resolved to desired level
   */
  
  def checkLevelAndThrowException(level : Center.ResolveLevel.Value, message : String) = {
    if(this.resolvingLevel < level) {
      val msg = "desired level: " + level + ". resolving level: " + this.resolvingLevel + " message: " + message 
      throw new RuntimeException(msg)
    }
  }
  
  /**
   * check whether we already resolved to desired level
   */
  
  def checkLevel(level : Center.ResolveLevel.Value) = this.resolvingLevel >= level
  
  /**
   * return resolving level
   */
  
  def getResolvingLevel = this.resolvingLevel
  
  /**
   * set resolving level
   */
  
  def setResolvingLevel(level : Center.ResolveLevel.Value) = {
    this.resolvingLevel = level
  }
}