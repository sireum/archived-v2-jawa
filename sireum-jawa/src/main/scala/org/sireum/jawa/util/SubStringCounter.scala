/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.util

import scala.annotation.tailrec

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */ 
object SubStringCounter {
	def countSubstring(str1:String, str2:String):Int={
   @tailrec def count(pos:Int, c:Int):Int={
	      val idx=str1 indexOf(str2, pos)
	      if(idx == -1) c else count(idx+str2.size, c+1)
	   }
	   count(0,0)
	}
}