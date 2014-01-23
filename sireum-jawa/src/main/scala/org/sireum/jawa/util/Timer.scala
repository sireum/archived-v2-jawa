package org.sireum.jawa.util

class Timer(limit : Long) {
	private val startTime = System.currentTimeMillis()
	def isTimeOut : Boolean = (System.currentTimeMillis() - this.startTime) > limit
	def isTimeOutAndThrow = if(isTimeOut) throw new TimeOutException
}

class TimeOutException extends Exception