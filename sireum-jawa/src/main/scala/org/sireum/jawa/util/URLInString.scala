package org.sireum.jawa.util

import java.net.URL
import java.net.MalformedURLException
import org.sireum.util._
import java.util.regex.Pattern
import java.util.regex.Matcher
 

object URLInString {
  def extract(str : String) : Set[String] = {
    val results = msetEmpty[String]
    val regex = "\\b(((ht|f)tp(s?)\\:\\/\\/|~\\/|\\/)|www.)" + 
            "(\\w+:\\w+@)?(([-\\w]+\\.)+(com|org|net|gov" + 
            "|mil|biz|info|mobi|name|aero|jobs|museum" + 
            "|travel|[a-z]{2}))(:[\\d]{1,5})?" + 
            "(((\\/([-\\w~!$+|.,=]|%[a-f\\d]{2})+)+|\\/)+|\\?|#)?" + 
            "((\\?([-\\w~!$+|.,*:]|%[a-f\\d{2}])+=?" +
            "([-\\w~!$+|.,*:=]|%[a-f\\d]{2})*)" + 
            "(&(?:[-\\w~!$+|.,*:]|%[a-f\\d{2}])+=?" + 
            "([-\\w~!$+|.,*:=]|%[a-f\\d]{2})*)*)*" + 
            "(#([-\\w~!$+|.,*:=]|%[a-f\\d]{2})*)?\\b"
    val p = Pattern.compile(regex)
    val m = p.matcher(str)
    while(m.find()) {
      var urlStr = m.group()
      if (urlStr.startsWith("(") && urlStr.endsWith(")"))
      {
        urlStr = urlStr.substring(1, urlStr.length() - 1)
      }
      results.add(urlStr)
    }
    results.toSet
  }
}