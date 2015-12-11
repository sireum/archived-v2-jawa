/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa

import org.sireum.util._

/**
 * @author fgwei
 */
case class JawaPackage(name: String, parent: Option[JawaPackage]) extends JavaKnowledge {
  def getPkgList: IList[JawaPackage] = {
    var pkgs: IList[JawaPackage] = List(this)
    var parentpkg = parent
    while(parentpkg.isDefined) {
      pkgs = parentpkg.get :: pkgs
      parentpkg = parentpkg.get.parent
    }
    pkgs
  }
  def getPkgNameList: IList[String] = {
    getPkgList.map(_.name)
  }
  def toPkgString(sep: String): String = {
    getPkgNameList.mkString(sep)
  }
}