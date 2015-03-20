/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.alir.callGraph

import org.sireum.util._
import java.io.Writer

class CallGraph {
  /**
   * map from procedures to it's callee procedures
   * map from caller sig to callee sigs
   */
  private val callMap : MMap[String, ISet[String]] = mmapEmpty
  
  def addCall(from : String, to : String) = this.callMap += (from -> (this.callMap.getOrElse(from, isetEmpty) + to))
  def addCalls(from : String, to : ISet[String]) = this.callMap += (from -> (this.callMap.getOrElse(from, isetEmpty) ++ to))
  
  def getCallMap : IMap[String, ISet[String]] = this.callMap.toMap

  def getReachableProcedures(procs : Set[String]) : Set[String] = {
    calculateReachableProcedures(procs, isetEmpty) ++ procs
  }
  
  private def calculateReachableProcedures(procs : Set[String], processed : Set[String]) : Set[String] = {
    if(procs.isEmpty) Set()
    else
      procs.map{
        proc =>
          if(processed.contains(proc)){
            Set[String]()
          } else {
            val callees = this.callMap.getOrElse(proc, isetEmpty)
            callees ++ calculateReachableProcedures(callees, processed + proc)
          }
      }.reduce((s1, s2) => s1 ++ s2)
  }
  
  def write(w : Writer) = {
    
  }
}