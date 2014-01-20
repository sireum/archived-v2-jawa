package org.sireum.jawa

trait LibraryAPISummary {
  
  /**
   * check given API name is present in library
   */
	def isLibraryAPI(apiName : String) : Boolean
}