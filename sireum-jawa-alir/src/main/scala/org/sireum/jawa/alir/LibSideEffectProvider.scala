package org.sireum.jawa.alir

import org.sireum.jawa.alir.sideEffectAnalysis.InterProceduralSideEffectAnalysisResult
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import org.sireum.jawa.xml.AndroidXStream
import org.sireum.util._
import org.sireum.jawa.JawaProcedure
import java.io.File

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
  
  def getInfluencedFields(position : Int, calleeSig : String) : ISet[String] = {
    require(isDefined)
    val resultopt = this.ipsear.result(calleeSig)
    resultopt match{
      case Some(result) => result.writeMap.getOrElse(position, isetEmpty)
      case None => Set("ALL")
    }
  }
}