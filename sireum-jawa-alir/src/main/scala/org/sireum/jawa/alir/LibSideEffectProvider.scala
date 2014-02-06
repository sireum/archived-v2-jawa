package org.sireum.jawa.alir

import org.sireum.jawa.alir.interProcedural.sideEffectAnalysis.InterProceduralSideEffectAnalysisResult
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import org.sireum.jawa.xml.AndroidXStream
import org.sireum.util._
import org.sireum.jawa.JawaProcedure

object LibSideEffectProvider {
  var ipsear : InterProceduralSideEffectAnalysisResult = null
	def init(ipsear : InterProceduralSideEffectAnalysisResult) = {
	  this.ipsear = ipsear
	}
  
  def init = {
    val reader = new GZIPInputStream(new FileInputStream("/Volumes/hd/fgwei/Stash/Amandroid/LibSummary/AndroidLibSideEffectResult.xml.zip"))
    val interPSEA = AndroidXStream.fromXml(reader).asInstanceOf[InterProceduralSideEffectAnalysisResult]
    reader.close()
    this.ipsear = interPSEA
  }
  
  def getInfluencedFields(position : Int, calleeSig : String) : ISet[String] = {
    val resultopt = this.ipsear.result(calleeSig)
    resultopt match{
      case Some(result) => result.readMap.getOrElse(position, isetEmpty) ++ result.writeMap.getOrElse(position, isetEmpty)
      case None => Set("ALL")
    }
  }
}