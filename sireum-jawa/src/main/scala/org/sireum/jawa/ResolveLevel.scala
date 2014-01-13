package org.sireum.jawa

trait ResolveLevel {
	/**
   * resolving level of current record
   */
  
  private var resolvingLevel : Center.ResolveLevel.Value = Center.ResolveLevel.NO
  
  /**
   * format level to String
   */
  
  private def levelToString(level : Center.ResolveLevel.Value) : String = {
    level match{
      case Center.ResolveLevel.NO => "NO"
      case Center.ResolveLevel.HIERARCHY => "HIERARCHY"
      case Center.ResolveLevel.BODY => "BODY"
    }
  }
  
  /**
   * check whether we already resolved to desired level
   */
  
  def checkLevel(level : Center.ResolveLevel.Value) = {
    if(this.resolvingLevel < level) {
      val msg = "desired level " + levelToString(level) + " is higher than resolving level " + levelToString(this.resolvingLevel)
      throw new RuntimeException(msg)
    }
  }
  
  /**
   * return resolving level
   */
  
  def getResolvingLevel = this.resolvingLevel
  
  /**
   * set resolving level
   */
  
  def setResolvingLevel(level : Center.ResolveLevel.Value) = this.resolvingLevel = level
}