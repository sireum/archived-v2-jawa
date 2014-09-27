/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.util

// limit is in minute
class Timer(limit : Int) {
	private val startTime = System.currentTimeMillis()
	def isTimeOut : Boolean = (System.currentTimeMillis() - this.startTime) > limit * 60000
	def isTimeOutAndThrow = if(isTimeOut) throw new TimeOutException
}

class TimeOutException extends Exception