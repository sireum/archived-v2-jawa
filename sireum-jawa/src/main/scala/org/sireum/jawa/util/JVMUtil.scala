/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.util

import java.io.InputStreamReader
import java.io.BufferedReader
import java.net.URLClassLoader

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object JVMUtil {
	def startSecondJVM[C](clazz : Class[C], javaHeapSize : String, args : List[String], redirectStream : Boolean) = {
    val separator = System.getProperty("file.separator")
    val classpath = Thread.currentThread().getContextClassLoader().asInstanceOf[URLClassLoader].getURLs().map(_.getPath()).reduce((c1, c2) => c1 + ":" + c2)
    
    val path = System.getProperty("java.home") + separator + "bin" + separator + "java"
    import scala.collection.JavaConversions._
    val commands : java.util.List[String] = List(path, javaHeapSize, "-cp", classpath, clazz.getCanonicalName().stripSuffix("$")) ::: args
    val processBuilder = new ProcessBuilder(commands)
    processBuilder.redirectErrorStream(redirectStream)
    val process = processBuilder.start()
    val is = process.getInputStream()
    val isr = new InputStreamReader(is)
    val br = new BufferedReader(isr)
    var line = br.readLine()
    while (line != null) {
      println(line)
      line = br.readLine()
    }
    process.waitFor()
  }
}