/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.util

import org.sireum.util._
import org.sireum.jawa.Type

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
class SignatureParser(sig : String) {
  
  private class ParameterSignatureIterator extends Iterator[String] {
        private var index = 1;

        def hasNext() : Boolean = {
            return index < signature.length() && signature.charAt(index) != ')';
        }

        def next() : String = {
            if (!hasNext())
                throw new NoSuchElementException();
            val result = new StringBuilder();
            var done : Boolean = false;
            do {
                done = true;
                val ch = signature.charAt(index);
                ch match {
                  case 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' =>
                      result.append(signature.charAt(index));
                      index+=1;
                  case 'L' =>
                      val semi = signature.indexOf(';', index + 1);
                      if (semi < 0)
                          throw new IllegalStateException("Invalid method signature: " + signature);
                      result.append(signature.substring(index, semi + 1));
                      index = semi + 1;
                  case '[' =>
                      result.append('[');
                      index+=1;
                      done = false;
                  case _ =>
                      throw new IllegalStateException("Invalid method signature: " + signature);
                }
            } while (!done);

            return result.toString();
        }

        def remove() = {
            throw new UnsupportedOperationException();
        }
    }
  
    /**
     * Get the method return type signature.
     * 
     * @return the method return type signature
     */
    def getReturnTypeSignature() : String = {
      val endOfParams = signature.lastIndexOf(')')
      if (endOfParams < 0)
        throw new IllegalArgumentException("Bad method signature: " + signature);
      return signature.substring(endOfParams + 1)
    }
    
    /**
     * Get the method return type. 
     * 
     * @return the method return type signature
     */
    def getReturnType() : Type = StringFormConverter.formatSigToTypeForm(getReturnTypeSignature)
    
    /**
     * Get the method return type. 
     * 
     * @return the method return type signature
     */
    def getReturnObjectType() : Option[Type] = {
      if(isReturnObject){
        val retPart = getReturnTypeSignature
        Some(StringFormConverter.formatSigToTypeForm(retPart))
      } else None
    }
    
    def isReturnNonNomal() : Boolean = {
      val ret = getReturnTypeSignature()
      ret.startsWith("L") || ret.startsWith("[")
    }
    
    def isReturnObject() : Boolean = {
      val ret = getReturnTypeSignature()
      ret.startsWith("L")
    }
    
    def isReturnArray() : Boolean = {
      val ret = getReturnTypeSignature()
      ret.startsWith("[")
    }
    
    def getReturnArrayDimension() : Int = {
      val ret = getReturnTypeSignature()
      if(ret.startsWith("["))
      	ret.lastIndexOf('[') - ret.indexOf('[') + 1
      else 0
    }
  
    def getParameters() : IList[String] = {
      var count = 0
      val params : MList[String] = mlistEmpty
      val iterator = new ParameterSignatureIterator()
      while(iterator.hasNext){
        val p = iterator.next()
        params.insert(count, p)
        count+=1
      }
      params.toList
    }
    
    def getParameterTypes() : List[Type] = {
      val params : MList[Type] = mlistEmpty
      val iterator = new ParameterSignatureIterator()
      while(iterator.hasNext){
        val p = StringFormConverter.formatSigToTypeForm(iterator.next())
        params += p
      }
      params.toList
    }
    
    def getParameterNum() : Int = {
      var count = 0
      val iterator = new ParameterSignatureIterator()
      while(iterator.hasNext){
        val p = iterator.next()
        count+=1
      }
      count
    }
    
    def getObjectParameters() : MMap[Int, Type] = {
      var count = 0
      val params : MMap[Int, Type] = mmapEmpty
      val iterator = new ParameterSignatureIterator()
      while(iterator.hasNext){
        val p = iterator.next()
        if(p.startsWith("L") || p.startsWith("[")){
        	params(count) = StringFormConverter.formatSigToTypeForm(p)
        }
        count+=1
      }
      params
    }
    
    /**
		 * get record name from procedure signature. e.g. Ljava/lang/Object;.equals:(Ljava/lang/Object;)Z -> java.lang.Object
		 */
    
    def getRecordName : String = StringFormConverter.getRecordNameFromProcedureSignature(this.signature)
    
    /**
     * before cut: LSavings;.interest:(I)V, after cut: (I)V
     */
    def getParamSig = {
      signature = signature.substring(signature.indexOf(':') + 1, signature.length())
      this
    }

    private var signature : String = sig
}