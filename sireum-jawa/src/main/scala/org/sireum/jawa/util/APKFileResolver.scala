/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.util

import org.sireum.util.FileResourceUri
import org.sireum.util.FileUtil
import java.util.zip._
import java.io.File
import java.io.FileOutputStream
import java.net.URL
import java.io.InputStream
import org.sireum.util.FileResourceUri
import java.io.FileInputStream
import java.net.URI

object APKFileResolver {
  
  /**
   * given an APK file uri, the following method returns the uri of the inner dex file
   */
	def getDexFile(apkUri : FileResourceUri, outputUri : FileResourceUri) : FileResourceUri = {
    //create directory
	  val apkFile = new File(new URI(apkUri))
	  val zipis = new ZipInputStream(new FileInputStream(apkFile))
    val dirName = apkFile.getName().substring(0, apkFile.getName().lastIndexOf("."))
    val outputDir = new File(new URI(outputUri + "/" + dirName))
    if(outputDir.exists()){
      MyFileUtil.deleteDir(outputDir)
    }
	  outputDir.mkdirs()
	  val outputFile = new File(outputDir + "/" + dirName + ".dex")
	  val ops = new FileOutputStream(outputFile)
    //resolve with apk file
    while(zipis.available() == 1){
      val ze = zipis.getNextEntry()
      if(ze != null)
	      if(ze.getName().endsWith(".dex")){
	        var reading = true
	        while(reading){
	          zipis.read() match {
	            case -1 => reading = false
	            case c => ops.write(c)
	          }
	        }
	      }
    }
	  ops.flush()
	  zipis.close()
	  FileUtil.toUri(outputFile)
	}
	
	def deleteOutputs(apkUri : FileResourceUri, outputUri : FileResourceUri) = {
	  val apkFile = new File(new URI(apkUri))
	  val dirName = apkFile.getName().substring(0, apkFile.getName().lastIndexOf("."))
    val outputDir = new File(new URI(outputUri + "/" + dirName))
    if(outputDir.exists()){
      MyFileUtil.deleteDir(outputDir)
    }
	}
}