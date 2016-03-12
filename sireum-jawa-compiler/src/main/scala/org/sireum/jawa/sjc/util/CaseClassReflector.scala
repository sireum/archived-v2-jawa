/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
package org.sireum.jawa.sjc.util

trait CaseClassReflector extends Product {

  def getFields: List[(String, Any)] = {
    val names = getClass.getDeclaredFields map { _.getName }
    names.toList zip productIterator.toList
  }

  private def getFieldsOld: List[(String, Any)] = {
    var fieldValueToName: Map[Any, String] = Map()
    for (field <- getClass.getDeclaredFields) {
      field.setAccessible(true)
      fieldValueToName += (field.get(this) -> field.getName)
    }
    productIterator.toList map { value => fieldValueToName(value) -> value }
  }

}
