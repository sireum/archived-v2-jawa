package org.sireum.jawa.sjc.log

import org.sireum.jawa.io.Position

trait Problem
{
  def category: String
  def severity: Severity.Value
  def message: String
  def position(): Position
}