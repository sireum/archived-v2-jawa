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
package org.sireum.jawa.io

case class Range(offset: Int, length: Int) {

  def contains(other: Range) = other.offset >= offset && other.offset + other.length <= offset + length

  def strictlyContains(other: Range) = (this contains other) && this.length > other.length

  /**
   * @return the smallest range that contains both this and other
   */
  def mergeWith(other: Range) = {
    val List(earliest, latest) = List(this, other) sortBy (_.offset)
    Range(earliest.offset, latest.offset - earliest.offset + latest.length)
  }

  def intersects(other: Range) =
    !(other.offset >= offset + length || other.offset + other.length - 1 < offset)

  def expandLeft(n: Int): Range = Range(offset - n, length + n)

}
