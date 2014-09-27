/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object MessageCenter {
	/**
	 * CRITICAL: Only print critical message
	 * NORMAL: Print some useful debug message
	 * VERBOSE: Print all message
	 * NO: Don't print any message
	 */
	object MSG_LEVEL extends Enumeration {
	  val NO, CRITICAL, NORMAL, VERBOSE = Value
	}
	var msglevel : MSG_LEVEL.Value = MSG_LEVEL.NORMAL
	var showLevel : Boolean = true
	var showTitle : Boolean = false

	private def printMSG(level : MSG_LEVEL.Value, title : String, msg : String, err : Boolean) = {
	  var tmp = ""
    if(showLevel) tmp += "[" + level + "]"
    if(showTitle) tmp += "[" + title + "]"
    tmp += msg
    if(err) System.err.println(tmp)
  	else println(tmp)
	}
	
	implicit def msg_critical(title : String, msg : String) = {
	  if(msglevel >= MSG_LEVEL.CRITICAL)
	    printMSG(MSG_LEVEL.CRITICAL, title, msg, false)
	}
	implicit def err_msg_critical(title : String, msg : String) = {
	  if(msglevel >= MSG_LEVEL.CRITICAL)
	  	printMSG(MSG_LEVEL.CRITICAL, title, msg, true)
	}
	implicit def msg_normal(title : String, msg : String) = {
	  if(msglevel >= MSG_LEVEL.NORMAL)
	  	printMSG(MSG_LEVEL.NORMAL, title, msg, false)
	}
	implicit def err_msg_normal(title : String, msg : String) = {
	  if(msglevel >= MSG_LEVEL.NORMAL)
	  	printMSG(MSG_LEVEL.NORMAL, title, msg, true)
	}
	implicit def msg_detail(title : String, msg : String) = {
	  if(msglevel >= MSG_LEVEL.VERBOSE)
	  	printMSG(MSG_LEVEL.VERBOSE, title, msg, false)
	}
	implicit def err_msg_detail(title : String, msg : String) = {
	  if(msglevel >= MSG_LEVEL.VERBOSE)
	  	printMSG(MSG_LEVEL.VERBOSE, title, msg, true)
	}
}