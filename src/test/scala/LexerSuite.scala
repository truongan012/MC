import org.scalatest.FunSuite

/**
  * Created by nhphung on 4/28/17.
  * Student name: Lam Truong An
  * Student ID: 1570733
  */
class LexerSuite extends FunSuite with TestLexer {

  test("A simple identifier") {
    val input = "abc"
    val expect = "abc,<EOF>"
    assert(checkLex(input, expect, 101))
  }
  test("Half function declare") {
    val input = "main int {"
    val expect = "main,int,{,<EOF>"
    assert(checkLex(input, expect, 102))
  }
  test("Open and close parentheses") {
    val input = "} int main {"
    val expect = "},int,main,{,<EOF>"
    assert(checkLex(input, expect, 103))
  }
  test("Type boolean") {
    val input = "boolean a ;"
    val expect = "boolean,a,;,<EOF>"
    assert(checkLex(input, expect, 104))
  }
  test("Assign false to boolean variable") {
    val input = "a = false"
    val expect = "a,=,false,<EOF>"
    assert(checkLex(input, expect, 105))
  }
  test("Assign true to boolean variable") {
    val input = "a = true"
    val expect = "a,=,true,<EOF>"
    assert(checkLex(input, expect, 106))
  }
  test("If statement") {
    val input = "if ( a == true ) b = 12 ;"
    val expect = "if,(,a,==,true,),b,=,12,;,<EOF>"
    assert(checkLex(input, expect, 107))
  }
  test("If else statement") {
    val input = "if ( ) else b = .5e12"
    val expect = "if,(,),else,b,=,.5e12,<EOF>"
    assert(checkLex(input, expect, 108))
  }
  test("Break statement") {
    val input = "break ;"
    val expect = "break,;,<EOF>"
    assert(checkLex(input, expect, 109))
  }
  test("Continue statement") {
    val input = "continue ;"
    val expect = "continue,;,<EOF>"
    assert(checkLex(input, expect, 110))
  }
  test("For statement") {
    val input = "for ( int a = 0 ; a < 3 ; a + 1 )"
    val expect = "for,(,int,a,=,0,;,a,<,3,;,a,+,1,),<EOF>"
    assert(checkLex(input, expect, 111))
  }
  test("return statement") {
    val input = "return 0 ;"
    val expect = "return,0,;,<EOF>"
    assert(checkLex(input, expect, 112))
  }
  test("Do While Statement") {
    val input = "do a = 23 ; while a > 0 ;"
    val expect = "do,a,=,23,;,while,a,>,0,;,<EOF>"
    assert(checkLex(input, expect, 113))
  }
  test("Type int, void") {
    val input = "int a ; void main ( )"
    val expect = "int,a,;,void,main,(,),<EOF>"
    assert(checkLex(input, expect, 114))
  }
  test("Type string") {
    val input = "string a ;"
    val expect = "string,a,;,<EOF>"
    assert(checkLex(input, expect, 115))
  }
  test("Type float") {
    val input = "float abcd = 1."
    val expect = "float,abcd,=,1.,<EOF>"
    assert(checkLex(input, expect, 116))
  }
  test("Add operator") {
    val input = "54 + 65"
    val expect = "54,+,65,<EOF>"
    assert(checkLex(input, expect, 117))
  }
  test("Sub operator") {
    val input = "234 - 59"
    val expect = "234,-,59,<EOF>"
    assert(checkLex(input, expect, 118))
  }
  test("Mul operator") {
    val input = "3.5 * 32"
    val expect = "3.5,*,32,<EOF>"
    assert(checkLex(input, expect, 119))
  }
  test("Div operator") {
    val input = "2.5 / 0.5"
    val expect = "2.5,/,0.5,<EOF>"
    assert(checkLex(input, expect, 120))
  }
  test("Mod operator") {
    val input = "2354335 % 32"
    val expect = "2354335,%,32,<EOF>"
    assert(checkLex(input, expect, 121))
  }
  test("Equal operator") {
    val input = "sbv == 12"
    val expect = "sbv,==,12,<EOF>"
    assert(checkLex(input, expect, 122))
  }
  test("Not equal operator") {
    val input = "abc != cba"
    val expect = "abc,!=,cba,<EOF>"
    assert(checkLex(input, expect, 123))
  }
  test("Not logic operator") {
    val input = "! true"
    val expect = "!,true,<EOF>"
    assert(checkLex(input, expect, 124))
  }
  test("Or logic operator") {
    val input = "true || false"
    val expect = "true,||,false,<EOF>"
    assert(checkLex(input, expect, 125))
  }
  test("And logic operator") {
    val input = "false && true"
    val expect = "false,&&,true,<EOF>"
    assert(checkLex(input, expect, 126))
  }
  test("Less than operator") {
    val input = "small < _MAX"
    val expect = "small,<,_MAX,<EOF>"
    assert(checkLex(input, expect, 127))
  }
  test("Greater than operator") {
    val input = "small2 > small1"
    val expect = "small2,>,small1,<EOF>"
    assert(checkLex(input, expect, 128))
  }
  test("Less than or equal operator") {
    val input = "temp_1 <= temp_1_temp"
    val expect = "temp_1,<=,temp_1_temp,<EOF>"
    assert(checkLex(input, expect, 129))
  }
  test("Greater than or equal operator") {
    val input = "foo ( ) >= goo ( int a )"
    val expect = "foo,(,),>=,goo,(,int,a,),<EOF>"
    assert(checkLex(input, expect, 130))
  }
  test("Assign operator") {
    val input = "abc = 6.1e23"
    val expect = "abc,=,6.1e23,<EOF>"
    assert(checkLex(input, expect, 131))
  }
  test("Left right brackets") {
    val input = "( a + b )"
    val expect = "(,a,+,b,),<EOF>"
    assert(checkLex(input, expect, 132))
  }
  test("Open and close brackets") {
    val input = "foo ) ("
    val expect = "foo,),(,<EOF>"
    assert(checkLex(input, expect, 133))
  }
  test("Left right parentheses") {
    val input = "if ( ) { }"
    val expect = "if,(,),{,},<EOF>"
    assert(checkLex(input, expect, 134))
  }
  test("Left right square brackets") {
    val input = "int a [ 7 ]"
    val expect = "int,a,[,7,],<EOF>"
    assert(checkLex(input, expect, 135))
  }
  test("Open and close square brackets") {
    val input = "inr a ] 7 ["
    val expect = "inr,a,],7,[,<EOF>"
    assert(checkLex(input, expect, 136))
  }
  test("Semicolon") {
    val input = "float a ;"
    val expect = "float,a,;,<EOF>"
    assert(checkLex(input, expect, 137))
  }
  test("Comma") {
    val input = "boolean a , b ;"
    val expect = "boolean,a,,,b,;,<EOF>"
    assert(checkLex(input, expect, 138))
  }
  test("Interger literals 1") {
    val input = "0"
    val expect = "0,<EOF>"
    assert(checkLex(input, expect, 139))
  }
  test("Interger literals 2") {
    val input = "02"
    val expect = "02,<EOF>"
    assert(checkLex(input, expect, 140))
  }
  test("Interger literals 3") {
    val input = "14564820"
    val expect = "14564820,<EOF>"
    assert(checkLex(input, expect, 141))
  }
  test("Float literals 1") {
    val input = "1.2"
    val expect = "1.2,<EOF>"
    assert(checkLex(input, expect, 142))
  }
  test("Float literals 2") {
    val input = "1."
    val expect = "1.,<EOF>"
    assert(checkLex(input, expect, 143))
  }
  test("Float literals 3") {
    val input = ".1"
    val expect = ".1,<EOF>"
    assert(checkLex(input, expect, 144))
  }
  test("Float literals 4") {
    val input = "1e2"
    val expect = "1e2,<EOF>"
    assert(checkLex(input, expect, 145))
  }
  test("Float literals 5") {
    val input = "1.2E-2"
    val expect = "1.2E-2,<EOF>"
    assert(checkLex(input, expect, 146))
  }
  test("Float literals 6") {
    val input = ".1E4"
    val expect = ".1E4,<EOF>"
    assert(checkLex(input, expect, 147))
  }
  test("Float literals 7") {
    val input = "9.0"
    val expect = "9.0,<EOF>"
    assert(checkLex(input, expect, 148))
  }
  test("Float literals 8") {
    val input = "123e3"
    val expect = "123e3,<EOF>"
    assert(checkLex(input, expect, 149))
  }
  test("Float literals 9") {
    val input = "0.33e-3"
    val expect = "0.33e-3,<EOF>"
    assert(checkLex(input, expect, 150))
  }
  test("Float literals 10") {
    val input = "265E-6"
    val expect = "265E-6,<EOF>"
    assert(checkLex(input, expect, 151))
  }
  test("Not Float literals 1") {
    val input = "1.e4"
    val expect = "1.,e4,<EOF>"
    assert(checkLex(input, expect, 152))
  }
  test("Not Float literals 2") {
    val input = "3e"
    val expect = "3,e,<EOF>"
    assert(checkLex(input, expect, 153))
  }
  test("Boolean literals 1") {
    val input = "true"
    val expect = "true,<EOF>"
    assert(checkLex(input, expect, 154))
  }
  test("Boolean literals 2") {
    val input = "false"
    val expect = "false,<EOF>"
    assert(checkLex(input, expect, 155))
  }
  test("String literals 1") {
    val input = "\"\""
    val expect = "\"\",<EOF>"
    assert(checkLex(input, expect, 156))
  }
  test("String literals 2") {
    val input = "\"Test the string\""
    val expect = "\"Test the string\",<EOF>"
    assert(checkLex(input, expect, 157))
  }
  test("String literals 3") {
    val input = "\"Test \\\'the\\\' string\""
    val expect = "\"Test \\\'the\\\' string\",<EOF>"
    assert(checkLex(input, expect, 158))
  }
  test("String literals 4") {
    val input = "\"Test \\\"the\\\" string\""
    val expect = "\"Test \\\"the\\\" string\",<EOF>"
    assert(checkLex(input, expect, 159))
  }
  test("String literals 5") {
    val input = "\"Test \\\\the\\\\ string\""
    val expect = "\"Test \\\\the\\\\ string\",<EOF>"
    assert(checkLex(input, expect, 160))
  }
  test("String literals 6") {
    val input = "\"Test \\tthe\\t string\""
    val expect = "\"Test \\tthe\\t string\",<EOF>"
    assert(checkLex(input, expect, 161))
  }
  test("String literals 7") {
    val input = "\"Test \\nthe\\n string\""
    val expect = "\"Test \\nthe\\n string\",<EOF>"
    assert(checkLex(input, expect, 162))
  }
  test("String literals 8") {
    val input = "\"Test \\rthe\\r string\""
    val expect = "\"Test \\rthe\\r string\",<EOF>"
    assert(checkLex(input, expect, 163))
  }
  test("String literals 9") {
    val input = "\"Test \\fthe\\f string\""
    val expect = "\"Test \\fthe\\f string\",<EOF>"
    assert(checkLex(input, expect, 164))
  }
  test("String literals 10") {
    val input = "\"Test \\bthe\\b string\""
    val expect = "\"Test \\bthe\\b string\",<EOF>"
    assert(checkLex(input, expect, 165))
  }
  test("String literals 11") {
    val input = "\""
    val expect = "Unclosed string: \""
    assert(checkLex(input, expect, 166))
  }
  test("String literals 12") {
    val input = "\"Test the string"
    val expect = "Unclosed string: \"Test the string"
    assert(checkLex(input, expect, 167))
  }
  test("String literals 13") {
    val input = "\"Test \\\'the\\\' string"
    val expect = "Unclosed string: \"Test \\\'the\\\' string"
    assert(checkLex(input, expect, 168))
  }
  test("String literals 14") {
    val input = "\"Test \\\"the\\\" string"
    val expect = "Unclosed string: \"Test \\\"the\\\" string"
    assert(checkLex(input, expect, 169))
  }
  test("String literals 15") {
    val input = "\"Test \\\\the\\\\ string"
    val expect = "Unclosed string: \"Test \\\\the\\\\ string"
    assert(checkLex(input, expect, 170))
  }
  test("String literals 16") {
    val input = "\"Test \\tthe\\t string"
    val expect = "Unclosed string: \"Test \\tthe\\t string"
    assert(checkLex(input, expect, 171))
  }
  test("String literals 17") {
    val input = "\"Test \\nthe\\n string"
    val expect = "Unclosed string: \"Test \\nthe\\n string"
    assert(checkLex(input, expect, 172))
  }
  test("String literals 18") {
    val input = "\"Test \\rthe\\r string"
    val expect = "Unclosed string: \"Test \\rthe\\r string"
    assert(checkLex(input, expect, 173))
  }
  test("String literals 19") {
    val input = "\"Test \\fthe\\f string"
    val expect = "Unclosed string: \"Test \\fthe\\f string"
    assert(checkLex(input, expect, 174))
  }
  test("String literals 20") {
    val input = "\"Test \\bthe\\b string"
    val expect = "Unclosed string: \"Test \\bthe\\b string"
    assert(checkLex(input, expect, 175))
  }
  test("String literals 21") {
    val input = "\"Test \'the\' string\""
    val expect = "Unclosed string: \"Test "
    assert(checkLex(input, expect, 176))
  }
  test("String literals 22") {
    val input = "\"Test \\athe\\a string\""
    val expect = "Illegal escape in string: \"Test \\athe\\a string\""
    assert(checkLex(input, expect, 177))
  }
  test("String literals 23") {
    val input = "\"Test \\vthe\\v string\""
    val expect = "Illegal escape in string: \"Test \\vthe\\v string\""
    assert(checkLex(input, expect, 178))
  }
  test("String literals 24") {
    val input = "\"Test \\gthe\\g string\""
    val expect = "Illegal escape in string: \"Test \\gthe\\g string\""
    assert(checkLex(input, expect, 179))
  }
  test("String literals 25") {
    val input = "\"Test \\ithe\\y string\""
    val expect = "Illegal escape in string: \"Test \\ithe\\y string\""
    assert(checkLex(input, expect, 180))
  }
  test("String literals 26") {
    val input = "\"Test \\sthe\\x string\""
    val expect = "Illegal escape in string: \"Test \\sthe\\x string\""
    assert(checkLex(input, expect, 181))
  }
  test("String literals 27") {
    val input = "\"Test \"the\" string\""
    val expect = "\"Test \",the,\" string\",<EOF>"
    assert(checkLex(input, expect, 182))
  }
  test("String literals 28") {
    val input = "\"Test \nthe\n string\""
    val expect = "Unclosed string: \"Test "
    assert(checkLex(input, expect, 183))
  }
  test("String literals 29") {
    val input = "\"Test \tthe\t string\""
    val expect = "\"Test \tthe\t string\",<EOF>"
    assert(checkLex(input, expect, 184))
  }
  test("String literals 30") {
    val input = "\"Test \\the\\ string\""
    val expect = "Illegal escape in string: \"Test \\the\\ string\""
    assert(checkLex(input, expect, 185))
  }
  test("String literals 31") {
    val input = "\"Test \rthe\r string\""
    val expect = "Unclosed string: \"Test "
    assert(checkLex(input, expect, 186))
  }
  test("String literals 32") {
    val input = "\"Test \fthe\f string\""
    val expect = "\"Test \fthe\f string\",<EOF>"
    assert(checkLex(input, expect, 187))
  }
  test("String literals 33") {
    val input = "\"Test \bthe\b string\""
    val expect = "\"Test \bthe\b string\",<EOF>"
    assert(checkLex(input, expect, 188))
  }
  test("Skip line comment 1") {
    val input =
      """//This is line comment
      int a ; //Declare variable a"""
    val expect = "int,a,;,<EOF>"
    assert(checkLex(input, expect, 189))
  }
  test("Skip line comment 2") {
    val input =
      """//This is line comment /*no special meaning*/
      int a ; //Declare variable a"""
    val expect = "int,a,;,<EOF>"
    assert(checkLex(input, expect, 190))
  }
  test("Skip block comment 1") {
    val input =
      """/*This is block comment
      Next line comment
      Last line comment*/
      int a ; //Declare variable a"""
    val expect = "int,a,;,<EOF>"
    assert(checkLex(input, expect, 191))
  }
  test("Skip block comment 2") {
    val input =
      """/*This is block comment
      Next line comment
      //Line comment is no special meaning
      Last line comment*/
      int a ; //Declare variable a"""
    val expect = "int,a,;,<EOF>"
    assert(checkLex(input, expect, 192))
  }
  test("Error character 1") {
    val input = "@"
    val expect = "ErrorToken @"
    assert(checkLex(input, expect, 193))
  }
  test("Error character 2") {
    val input = "#"
    val expect = "ErrorToken #"
    assert(checkLex(input, expect, 194))
  }
  test("Error character 3") {
    val input = "~"
    val expect = "ErrorToken ~"
    assert(checkLex(input, expect, 195))
  }
  test("Error character 4") {
    val input = "^"
    val expect = "ErrorToken ^"
    assert(checkLex(input, expect, 196))
  }
  test("Error character 5") {
    val input = "&"
    val expect = "ErrorToken &"
    assert(checkLex(input, expect, 197))
  }
  test("Error character 6") {
    val input = "'"
    val expect = "ErrorToken '"
    assert(checkLex(input, expect, 198))
  }
  test("Error character 7") {
    val input = "\\"
    val expect = "ErrorToken \\"
    assert(checkLex(input, expect, 199))
  }
  test("Error character 8") {
    val input = "$"
    val expect = "ErrorToken $"
    assert(checkLex(input, expect, 200))
  }

}