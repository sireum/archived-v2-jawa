/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa

import org.sireum.util.IList

/**
 * @author fgwei
 */
case class JawaPackage(name: String, parents: IList[JawaPackage]) extends JavaKnowledge {
  def toPkgString(sep: String): String = {
    (parents.map(_.name) :+ name).mkString(sep)
  }
}