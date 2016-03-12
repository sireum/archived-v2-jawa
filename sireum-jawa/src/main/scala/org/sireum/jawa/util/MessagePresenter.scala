/*******************************************************************************
 * Copyright (c) 2013 - 2016 Fengguo Wei and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Detailed contributors are listed in the CONTRIBUTOR.md
 ******************************************************************************/
//package org.sireum.jawa.util
//
///**
// * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
// */
//object MessagePresenter {
//  /**
//   * CRITICAL: Only print critical message
//   * INFO: Print some useful debug message
//   * VERBOSE: Print all message
//   * NO: Don't print any message
//   */
//  object MSG_LEVEL extends Enumeration {
//    val NO, CRITICAL, INFO, VERBOSE = Value
//  }
//  var msglevel : MSG_LEVEL.Value = MSG_LEVEL.CRITICAL
//  var showLevel : Boolean = true
//  var showTitle : Boolean = true
//
//  private def printMSG(level : MSG_LEVEL.Value, title : String, msg : String, err : Boolean) = {
//    val sb = new StringBuilder
//    if(showLevel) sb.append("[" + level + "]")
//    if(showTitle) sb.append("[" + title + "]")
//    sb.append(msg)
//    if(err) System.err.println(sb.toString())
//    else println(sb.toString())
//  }
//  
//  implicit def msg_critical(title : String, msg : String) = {
//    if(msglevel >= MSG_LEVEL.CRITICAL)
//      printMSG(MSG_LEVEL.CRITICAL, title, msg, false)
//  }
//  implicit def err_msg_critical(title : String, msg : String) = {
//    if(msglevel >= MSG_LEVEL.CRITICAL)
//      printMSG(MSG_LEVEL.CRITICAL, title, msg, true)
//  }
//  implicit def msg_info(title : String, msg : String) = {
//    if(msglevel >= MSG_LEVEL.INFO)
//      printMSG(MSG_LEVEL.INFO, title, msg, false)
//  }
//  implicit def err_msg_info(title : String, msg : String) = {
//    if(msglevel >= MSG_LEVEL.INFO)
//      printMSG(MSG_LEVEL.INFO, title, msg, true)
//  }
//  implicit def msg_detail(title : String, msg : String) = {
//    if(msglevel >= MSG_LEVEL.VERBOSE)
//      printMSG(MSG_LEVEL.VERBOSE, title, msg, false)
//  }
//  implicit def err_msg_detail(title : String, msg : String) = {
//    if(msglevel >= MSG_LEVEL.VERBOSE)
//      printMSG(MSG_LEVEL.VERBOSE, title, msg, true)
//  }
//}
