/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Main Contributors:
 *    Fengguo Wei - Argus Lab @ University of South Florida
 *    Sankardas Roy - Bowling Green State University
 *    
 * Contributors:
 *    Robby - Santos Lab @ Kansas State University
 *    Wu Zhou - Fireeye
 *    Fengchi Lin - Chinese People's Public Security University
 ******************************************************************************/
package org.sireum.jawa.alir

import org.sireum.jawa.alir.sideEffectAnalysis.InterProceduralSideEffectAnalysisResult
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import org.sireum.jawa.xml.AndroidXStream
import org.sireum.util._
import java.io.File
import org.sireum.jawa.Signature

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 * @author <a href="mailto:sroy@k-state.edu">Sankardas Roy</a>
 */ 
object LibSideEffectProvider {
  var ipsear : InterProceduralSideEffectAnalysisResult = null
	def init(ipsear : InterProceduralSideEffectAnalysisResult) = {
	  this.ipsear = ipsear
	}
  
  def init(zipFile : File) : Unit = {
    val reader = new GZIPInputStream(new FileInputStream(zipFile))
    val interPSEA = AndroidXStream.fromXml(reader).asInstanceOf[InterProceduralSideEffectAnalysisResult]
    reader.close()
    this.ipsear = interPSEA
  }
  
//  def init : Unit = {
//    
//    init(AndroidGlobal"/LibSummary/AndroidLibSideEffectResult.xml.zip")
//  }
  
  def isDefined : Boolean = ipsear != null
  
  def getInfluencedFields(position : Int, calleeSig : Signature) : ISet[String] = {
    require(isDefined)
    val resultopt = this.ipsear.result(calleeSig)
    resultopt match{
      case Some(result) => result.writeMap.getOrElse(position, isetEmpty)
      case None => Set("ALL")
    }
  }
}
