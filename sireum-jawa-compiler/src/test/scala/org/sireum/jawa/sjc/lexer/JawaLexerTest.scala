package org.sireum.jawa.sjc.lexer

import org.sireum.jawa._
import org.sireum.jawa.sjc.lexer.Tokens._
import org.scalatest._

import java.io._

class JawaLexerTest extends FlatSpec with ShouldMatchers {

  implicit def string2TestString(s: String) =
    new TestString(s)

  "" producesTokens ()

  """`format`""" producesTokens (ID)

  "`format`;`format`" producesTokens (ID, SEMI, ID)

  "|||" producesTokens (OP)

  ":=" producesTokens (ASSIGN_OP)

  "^~" producesTokens (OP)

  "v0/2" producesTokens (ID, OP, INTEGER_LITERAL)

  "record" producesTokens (CLASS_OR_INTERFACE)

  "procedure" producesTokens (METHOD)

  "foo  bar   baz" producesTokens (ID, WS, ID, WS, ID)

  "  " producesTokens (WS)

  "// comment" producesTokens (LINE_COMMENT)

  "//" producesTokens (LINE_COMMENT)

  "foo// comment" producesTokens (ID, LINE_COMMENT)

  "foo // comment" producesTokens (ID, WS, LINE_COMMENT)

  """foo// comment
    abc//comment""" producesTokens (ID, LINE_COMMENT, WS, ID, LINE_COMMENT)

  "foo/* comment */bar" producesTokens (ID, MULTILINE_COMMENT, ID)

  "/* bar var */" producesTokens (MULTILINE_COMMENT)

  "/**/" producesTokens (MULTILINE_COMMENT)

  "`yield`" producesTokens (ID)

  """"foobar"""" producesTokens (STRING_LITERAL)
  
  """`@@global`""" producesTokens (STATIC_ID)
  
  """@@global""" producesTokens (STATIC_ID)

  "\"\"\"f\"o\"o\"\"\"" producesTokens (STRING_LITERAL)

  """"\""""" producesTokens (STRING_LITERAL)

  "foo.bar.baz()" producesTokens (ID, DOT, ID, DOT, ID, LPAREN, RPAREN)

  ".1234" producesTokens (FLOATING_POINT_LITERAL)
  ".1234e2" producesTokens (FLOATING_POINT_LITERAL)
  ".1234e+2" producesTokens (FLOATING_POINT_LITERAL)
  ".1e-2" producesTokens (FLOATING_POINT_LITERAL)
  ".1e+2345f" producesTokens (FLOATING_POINT_LITERAL)
  ".1e+2345d" producesTokens (FLOATING_POINT_LITERAL)

  "100" producesTokens (INTEGER_LITERAL)
  "1" producesTokens (INTEGER_LITERAL)
  "1L" producesTokens (INTEGER_LITERAL)
  "0" producesTokens (INTEGER_LITERAL)
  "0L" producesTokens (INTEGER_LITERAL)
  "0x2345" producesTokens (INTEGER_LITERAL)
  "0x1" producesTokens (INTEGER_LITERAL)
  "0x32413L" producesTokens (INTEGER_LITERAL)
  
  "#" producesTokens (LOCATION_ID)
  "#L00011." producesTokens (LOCATION_ID)

  "0.1234" producesTokens (FLOATING_POINT_LITERAL)
  "0.1234e2" producesTokens (FLOATING_POINT_LITERAL)
  "0.1234e+2" producesTokens (FLOATING_POINT_LITERAL)
  "0.1e-2" producesTokens (FLOATING_POINT_LITERAL)
  "0.1e+2345f" producesTokens (FLOATING_POINT_LITERAL)
  "0.1e+2345d" producesTokens (FLOATING_POINT_LITERAL)

  "10e2" producesTokens (FLOATING_POINT_LITERAL)
  "10e+2" producesTokens (FLOATING_POINT_LITERAL)
  "10e-2" producesTokens (FLOATING_POINT_LITERAL)
  "10e+2345f" producesTokens (FLOATING_POINT_LITERAL)
  "10e+2345d" producesTokens (FLOATING_POINT_LITERAL)

  "'f'" producesTokens (CHARACTER_LITERAL)
  """'\n'""" producesTokens (CHARACTER_LITERAL)
  """'\025'""" producesTokens (CHARACTER_LITERAL)

  "#L0001. tokenTextBuffer:= new StringBuilder" producesTokens (LOCATION_ID, WS, ID, ASSIGN_OP, WS, NEW, WS, ID)

  "#Lx. cmp(v0, v1);" producesTokens (LOCATION_ID, WS, CMP, LPAREN, ID, COMMA, WS, ID, RPAREN, SEMI)
  
  """println("bob")
println("foo")""" producesTokens (ID, LPAREN, STRING_LITERAL, RPAREN, WS, ID, LPAREN, STRING_LITERAL, RPAREN)

  "\"\\u0061\"" producesTokens (STRING_LITERAL)
  "\"\\u000a\"" producesTokens (STRING_LITERAL)

 
  "0X1234" producesTokens (INTEGER_LITERAL)

  
  "\"\\u001A\"" producesTokens (STRING_LITERAL)

  "\"\"\"\\u001A\"\"\"" producesTokens (STRING_LITERAL)

  "foo+\\u0061+bar" producesTokens (ID, OP, ID, OP, ID)

  "-5f.max(2)" producesTokens (FLOATING_POINT_LITERAL, DOT, ID, LPAREN, INTEGER_LITERAL, RPAREN)
  
  "Lexer" should "throw a lexer exception" in {
    evaluating { JawaLexer.rawTokenise("\"\"\"", None) } should produce[JawaLexerException]
  }

"""
record `com.ksu.passwordPassTest.MainActivity`  @type class @AccessFlag PUBLIC  extends `android.app.Activity` {
      `android.widget.EditText` `com.ksu.passwordPassTest.MainActivity.editText`    @AccessFlag ;
      `android.widget.Button` `com.ksu.passwordPassTest.MainActivity.passButton`    @AccessFlag ;
   }
    procedure `void` `com.ksu.passwordPassTest.MainActivity.<init>` (`com.ksu.passwordPassTest.MainActivity` v1 @type `this`) @owner `com.ksu.passwordPassTest.MainActivity` @signature `Lcom/ksu/passwordPassTest/MainActivity;.<init>:()V` @Access `PUBLIC_CONSTRUCTOR` {
      temp ;
        v0;
      
#L047178.   v0:= 0I  @length `4`;
#L04717a.   call temp:=  `android.app.Activity.<init>`(v1) @signature `Landroid/app/Activity;.<init>:()V` @classDescriptor `android.app.Activity` @type direct;
#L047180.   v1.`com.ksu.passwordPassTest.MainActivity.editText`  := v0 @type `object`;
#L047184.   v1.`com.ksu.passwordPassTest.MainActivity.passButton`  := v0 @type `object`;
#L047188.   return @void ;

   }
""" producesTokens 
  (WS, CLASS_OR_INTERFACE, WS, ID, WS, AT, ID, WS, ID, WS, AT, ID, WS, ID, WS, EXTENDS_AND_IMPLEMENTS, WS, ID, WS, LBRACE,
   WS, ID, WS, ID, WS, AT, ID, WS, SEMI,
   WS, ID, WS, ID, WS, AT, ID, WS, SEMI,
   WS, RBRACE,
   WS, METHOD, WS, ID, WS, ID, WS, LPAREN, ID, WS, ID, WS, AT, ID, WS, ID, RPAREN, WS, AT, ID, WS, ID, WS, AT, ID, WS, ID, WS, AT, ID, WS, ID, WS, LBRACE,
   WS, ID, WS, SEMI,
   WS, ID, SEMI,
   WS,
   LOCATION_ID, WS, ID, ASSIGN_OP, WS, INTEGER_LITERAL, WS, AT, ID, WS, ID, SEMI, WS,
   LOCATION_ID, WS, CALL, WS, ID, ASSIGN_OP, WS, ID, LPAREN, ID, RPAREN, WS, AT, ID, WS, ID, WS, AT, ID, WS, ID, WS, AT, ID, WS, ID, SEMI, WS,
   LOCATION_ID, WS, ID, DOT, ID, WS, ASSIGN_OP, WS, ID, WS, AT, ID, WS, ID, SEMI, WS,
   LOCATION_ID, WS, ID, DOT, ID, WS, ASSIGN_OP, WS, ID, WS, AT, ID, WS, ID, SEMI, WS,
   LOCATION_ID, WS, RETURN, WS, AT, ID, WS, SEMI,
   WS,
   RBRACE, WS)

  class TestString(s: String) {

    def producesTokens(toks: TokenType*)() {
      check(s.stripMargin, toks.toList)
    }

    private def check(s: String, expectedTokens: List[TokenType]) {
      it should ("tokenise >>>" + s + "<<< as >>>" + expectedTokens + "<<<") in {
        val actualTokens: List[Token] = JawaLexer.rawTokenise(s, None)
        val actualTokenTypes = actualTokens.map(_.tokenType)
        require(actualTokenTypes.last == EOF, "Last token must be EOF, but was " + actualTokens.last.tokenType)
        require(actualTokenTypes.count(_ == EOF) == 1, "There must only be one EOF token")
        val reconstitutedSource = actualTokens.init.map(_.text).mkString
        require(actualTokenTypes.init == expectedTokens, "Tokens do not match. Expected " + expectedTokens + ", but was " + actualTokenTypes.init)
        require(s == reconstitutedSource, "tokens do not partition text correctly: " + s + " vs " + reconstitutedSource)
      }
    }

  }

}
