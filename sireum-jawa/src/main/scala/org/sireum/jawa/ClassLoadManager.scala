package org.sireum.jawa

import org.sireum.util._
import scala.collection.immutable.BitSet

object ClassLoadManager {
  /**
   * set of classes can be loaded by the program
   */
	private var classes : IList[JawaRecord] = ilistEmpty
	
	def reset = classes = ilistEmpty
	
	protected def addClass(clazz : JawaRecord) = {
	  this.classes :+= clazz
	}
	
	def getClassPosition(clazz : JawaRecord) : Int = {
	  if(!this.classes.contains(clazz)) addClass(clazz)
	  this.classes.indexOf(clazz)
	}
	
	def loadClass(clazz : JawaRecord) : BitSet = {
	  val position = getClassPosition(clazz)
	  BitSet(position)
	}
	
	def loadClass(clazz : JawaRecord, bitset : BitSet) : BitSet = {
	  require(!isLoaded(clazz, bitset))
	  val position = getClassPosition(clazz)
	  if(position < 0){
	    println("position:" + position)
	    return bitset
	  }
	  bitset + position
	}
	
	def isLoaded(clazz : JawaRecord, bitset : BitSet) : Boolean = {
	  val position = getClassPosition(clazz)
	  bitset(position)
	}
}