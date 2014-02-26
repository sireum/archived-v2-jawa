package org.sireum.jawa.util

import org.sireum.pilar.ast._
import org.sireum.util._

object ASTUtil {
	def getCallArgs(jumploc : JumpLocation) : List[String] = {
	  val argNames : MList[String] = mlistEmpty
    jumploc.jump match {
      case t : CallJump if t.jump.isEmpty =>
        t.callExp.arg match {
          case te : TupleExp =>
            val exps = te.exps
            for(i <- 0 to (exps.size-1)) {
              val varName = exps(i) match{
                case ne : NameExp => ne.name.name
                case a => a.toString()
              }
              argNames += varName
            }
          case _ =>
        }
      case _ =>
    }
	  argNames.toList
	}
}