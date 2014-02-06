package org.sireum.jawa.util

import scala.annotation.tailrec

object SubStringCounter {
	def countSubstring(str1:String, str2:String):Int={
   @tailrec def count(pos:Int, c:Int):Int={
	      val idx=str1 indexOf(str2, pos)
	      if(idx == -1) c else count(idx+str2.size, c+1)
	   }
	   count(0,0)
	}
}