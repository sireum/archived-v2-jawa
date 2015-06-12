/*
Copyright (c) 2013-2014 Fengguo Wei & Sankardas Roy, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/
package org.sireum.jawa.sjc.parser

import org.scalatest._
import org.sireum.jawa.sjc.lexer.JawaLexer
import org.sireum.util.FileUtil
import org.sireum.jawa.sjc.io.PlainFile
import java.io.BufferedReader
import java.io.StringReader
import org.sireum.jawa.sjc.io.AbstractFile
import org.sireum.jawa.sjc.util.FgSourceFile
import org.sireum.jawa.sjc.DefaultReporter
import org.sireum.util._

class JawaParserTest extends FlatSpec with ShouldMatchers {

  "Parser" should "not throw a parse exception" in {
    parseLocation("#Lx. if v0 != 0 then goto Ly; }")
  }
  
  "Parser" should "throw a parse exception" in {
    evaluating { parseLocation("if v0 == 0 then goto Ly; }") } should produce[JawaParserException]
  }
  
  "Parser" should "not throw a parse exception on monitor" in {
    parseLocation("#Lx. @monitorenter v0;")
  }

  "Parser" should "not throw an exception" in {
    parseLocation("""
#L1.   switch  v7
                 | 1 => goto Lx
                 | else => goto Ly;
""")
  }
  
  "Parser" should "not throw an exception on big program" in {
    parseCompilationUnit("""
record `b.a.a.a`  @type class @AccessFlag PUBLIC_FINAL extends  `java.io.Externalizable` @type interface, `java.lang.Cloneable` @type interface{
      `java.lang.String` `b.a.a.a.f`    @AccessFlag PRIVATE;
      `java.lang.Class` `b.a.a.a.g`    @AccessFlag PRIVATE;
      `b.a.a.d` `b.a.a.a.h`    @AccessFlag PRIVATE;
   }
      global `b.a.a.a` `@@b.a.a.a.a`    @AccessFlag PRIVATE_STATIC;
      global `b.a.a.a` `@@b.a.a.a.b`    @AccessFlag PRIVATE_STATIC;
      global `b.a.a.a` `@@b.a.a.a.c`    @AccessFlag PRIVATE_STATIC;
      global `java.lang.String`[] `@@b.a.a.a.d`    @AccessFlag PRIVATE_STATIC_FINAL;
      global `b.a.a.a` `@@b.a.a.a.e`    @AccessFlag PRIVATE_STATIC;
    procedure `void` `b.a.a.a.<clinit>` () @owner `b.a.a.a` @signature `Lb/a/a/a;.<clinit>:()V` @Access `STATIC_CONSTRUCTOR` {
      temp ;
        v0;
        v1;
        v2;
      
#L013a90.   v0:= new `b.a.a.a`;
#L013a94.   v1:= "text/plain; charset=unicode; class=java.io.InputStream" @type `object`;
#L013a98.   v2:= "Plain Text" @type `object`;
#L013a9c.   call temp:=  `b.a.a.a.<init>`(v0, v1, v2) @signature `Lb/a/a/a;.<init>:(Ljava/lang/String;Ljava/lang/String;)V` @classDescriptor `b.a.a.a` @type direct;
#L013aa2.   `@@b.a.a.a.a` := v0  @type `object`;
#L013aa6.   v0:= new `b.a.a.a`;
#L013aaa.   v1:= "application/x-java-serialized-object; class=java.lang.String" @type `object`;
#L013aae.   v2:= "Unicode String" @type `object`;
#L013ab2.   call temp:=  `b.a.a.a.<init>`(v0, v1, v2) @signature `Lb/a/a/a;.<init>:(Ljava/lang/String;Ljava/lang/String;)V` @classDescriptor `b.a.a.a` @type direct;
#L013ab8.   `@@b.a.a.a.b` := v0  @type `object`;
#L013abc.   v0:= new `b.a.a.a`;
#L013ac0.   v1:= "application/x-java-file-list; class=java.util.List" @type `object`;
#L013ac4.   v2:= "application/x-java-file-list" @type `object`;
#L013ac8.   call temp:=  `b.a.a.a.<init>`(v0, v1, v2) @signature `Lb/a/a/a;.<init>:(Ljava/lang/String;Ljava/lang/String;)V` @classDescriptor `b.a.a.a` @type direct;
#L013ace.   `@@b.a.a.a.c` := v0  @type `object`;
#L013ad2.   v0:= 16I  @length `16`;
#L013ad6.   v0:= new `java.lang.String`[v0];
#L013ada.   v1:= 0I  @length `4`;
#L013adc.   v2:= "text/sgml" @type `object`;
#L013ae0.   v0[v1]:= v2  @type `object`;
#L013ae4.   v1:= 1I  @length `4`;
#L013ae6.   v2:= "text/xml" @type `object`;
#L013aea.   v0[v1]:= v2  @type `object`;
#L013aee.   v1:= 2I  @length `4`;
#L013af0.   v2:= "text/html" @type `object`;
#L013af4.   v0[v1]:= v2  @type `object`;
#L013af8.   v1:= 3I  @length `4`;
#L013afa.   v2:= "text/rtf" @type `object`;
#L013afe.   v0[v1]:= v2  @type `object`;
#L013b02.   v1:= 4I  @length `4`;
#L013b04.   v2:= "text/enriched" @type `object`;
#L013b08.   v0[v1]:= v2  @type `object`;
#L013b0c.   v1:= 5I  @length `4`;
#L013b0e.   v2:= "text/richtext" @type `object`;
#L013b12.   v0[v1]:= v2  @type `object`;
#L013b16.   v1:= 6I  @length `4`;
#L013b18.   v2:= "text/uri-list" @type `object`;
#L013b1c.   v0[v1]:= v2  @type `object`;
#L013b20.   v1:= 7I  @length `4`;
#L013b22.   v2:= "text/tab-separated-values" @type `object`;
#L013b26.   v0[v1]:= v2  @type `object`;
#L013b2a.   v1:= 8I  @length `16`;
#L013b2e.   v2:= "text/t140" @type `object`;
#L013b32.   v0[v1]:= v2  @type `object`;
#L013b36.   v1:= 9I  @length `16`;
#L013b3a.   v2:= "text/rfc822-headers" @type `object`;
#L013b3e.   v0[v1]:= v2  @type `object`;
#L013b42.   v1:= 10I  @length `16`;
#L013b46.   v2:= "text/parityfec" @type `object`;
#L013b4a.   v0[v1]:= v2  @type `object`;
#L013b4e.   v1:= 11I  @length `16`;
#L013b52.   v2:= "text/directory" @type `object`;
#L013b56.   v0[v1]:= v2  @type `object`;
#L013b5a.   v1:= 12I  @length `16`;
#L013b5e.   v2:= "text/css" @type `object`;
#L013b62.   v0[v1]:= v2  @type `object`;
#L013b66.   v1:= 13I  @length `16`;
#L013b6a.   v2:= "text/calendar" @type `object`;
#L013b6e.   v0[v1]:= v2  @type `object`;
#L013b72.   v1:= 14I  @length `16`;
#L013b76.   v2:= "application/x-java-serialized-object" @type `object`;
#L013b7a.   v0[v1]:= v2  @type `object`;
#L013b7e.   v1:= 15I  @length `16`;
#L013b82.   v2:= "text/plain" @type `object`;
#L013b86.   v0[v1]:= v2  @type `object`;
#L013b8a.   `@@b.a.a.a.d` := v0  @type `object`;
#L013b8e.   v0:= 0I  @length `4`;
#L013b90.   `@@b.a.a.a.e` := v0  @type `object`;
#L013b94.   return @void ;

   }
    """)
  }

  val dirUri = FileUtil.toUri("/Users/fgwei/Developer/playground/androidlib/5.0")
  val filelist = FileUtil.listFiles(dirUri, "pilar", true)
  
  
  "Parser" should "not throw an exception on those files" in {
    filelist.foreach{
      fileUri =>
        val jf = new FgSourceFile(new PlainFile(FileUtil.toFile(fileUri)))
        val cu = parseCompilationUnit(jf.file)
        val oText = jf.file.text
        val newText = cu.toCode
        val reader1 = new BufferedReader(new StringReader(oText));
        val reader2 = new BufferedReader(new StringReader(newText));
        var line1 = reader1.readLine()
        var line2 = reader2.readLine()
				while(line1 != null && line2 != null){
          if(!line1.startsWith(line2)){
            throw new RuntimeException
          }
          line1 = reader1.readLine()
          line2 = reader2.readLine()
        }
    }
  }
  val reporter = new DefaultReporter
  private def parser(s: Either[String, AbstractFile]) = new JawaParser(JawaLexer.tokenise(s, reporter).toArray, reporter)
  private def parseLocation(s: String) = {
    val loc = parser(Left(s)).location
    if(reporter.hasErrors) throw new RuntimeException(reporter.problems.toString())
    loc
  }
  private def parseCompilationUnit(s: AbstractFile) = {
    val cu = parser(Right(s)).compilationUnit(true)
    val allAsts = cu.getAllChildrenInclude
    allAsts.foreach {
      ast =>
        if(!ast.isInstanceOf[CompilationUnit]) require(ast.enclosingTopLevelClass != null, ast + " should have top level class.")
    }
    if(reporter.hasErrors) throw new RuntimeException(reporter.problems.toString())
    cu
  }
  private def parseCompilationUnit(s: String) = {
    val cu = parser(Left(s)).compilationUnit(true)
    val allAsts = cu.getAllChildrenInclude
    allAsts.foreach {
      ast =>
        if(!ast.isInstanceOf[CompilationUnit]) require(ast.enclosingTopLevelClass != null, ast + " should have top level class.")
    }
    if(reporter.hasErrors) throw new RuntimeException(reporter.problems.toString())
    cu
  }

}
