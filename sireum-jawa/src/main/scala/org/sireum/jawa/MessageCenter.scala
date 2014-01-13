package org.sireum.jawa

object MessageCenter {
	/**
	 * CRITICAL: Only print critical message
	 * NORMAL: Print some useful debug message
	 * DETAIL: Print all message
	 */
	object MSG_LEVEL extends Enumeration {
	  val NO, CRITICAL, NORMAL, VERBOSE = Value
	}
	var msglevel : MSG_LEVEL.Value = MSG_LEVEL.CRITICAL

	implicit def msg_critical(msg : String) = {
	  if(msglevel >= MSG_LEVEL.CRITICAL)
	  	println("[CRITICAL]" + msg)
	}
	implicit def err_msg_critical(msg : String) = {
	  if(msglevel >= MSG_LEVEL.CRITICAL)
	  	System.err.println("[ERROR_CRITICAL]" + msg)
	}
	implicit def msg_normal(msg : String) = {
	  if(msglevel >= MSG_LEVEL.NORMAL)
	  	println("[NORMAL]" + msg)
	}
	implicit def err_msg_normal(msg : String) = {
	  if(msglevel >= MSG_LEVEL.NORMAL)
	  	System.err.println("[ERROR_NORMAL]" + msg)
	}
	implicit def msg_detail(msg : String) = {
	  if(msglevel >= MSG_LEVEL.VERBOSE)
	  	println("[VERBOSE]" + msg)
	}
	implicit def err_msg_detail(msg : String) = {
	  if(msglevel >= MSG_LEVEL.VERBOSE)
	  	System.err.println("[ERROR_VERBOSE]" + msg)
	}
}