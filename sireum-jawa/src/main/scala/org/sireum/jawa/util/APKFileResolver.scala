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
	def getDexFile(apkUri : FileResourceUri) : FileResourceUri = {
    //create directory
	  val apkFile = new File(new URI(apkUri))
	  val zipis = new ZipInputStream(new FileInputStream(apkFile))
	  val dir = apkFile.getParent()
    val dirName = apkFile.getName().substring(0, apkFile.getName().lastIndexOf("."))
    val dirc = new File(dir + "/" + dirName)
    if(!dirc.exists()){
      dirc.mkdirs()
    }
	  val ops = new FileOutputStream(dirc + "/" + dirName + ".dex")
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
    FileUtil.toUri(dirc + "/" + dirName + ".dex")
	}
}