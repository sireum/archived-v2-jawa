package org.sireum.jawa.util

// limit is in minute
class Timer(limit : Int) {
	private val startTime = System.currentTimeMillis()
	def isTimeOut : Boolean = (System.currentTimeMillis() - this.startTime) > limit * 60000
	def isTimeOutAndThrow = if(isTimeOut) throw new TimeOutException
}

class TimeOutException extends Exception