import org.scalatest.FunSuite

/**
  * Created by nhphung on 4/28/17.
  */
class ParserSuite extends FunSuite with TestParser {

  test("a simple program") {
    val input = "int main () {}"
    val expect = "sucessful"
    assert(checkRec(input, expect, 201))
  }
  test("more complex program") {
    val input = """int main () {
 putIntLn(4);
}"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 202))
  }
  test("wrong program") {
    val input = "} int main {"
    val expect = "Error on line 1 col 1: }"
    assert(checkRec(input, expect, 203))
  }
  test("a simple variable declaration") {
    val input = "int a;"
    val expect = "sucessful"
    assert(checkRec(input, expect, 204))
  }
  test("a array variable declaration") {
    val input = "int a[3];"
    val expect = "sucessful"
    assert(checkRec(input, expect, 205))
  }
  test("a simple variable declaration list") {
    val input = "int a, b;"
    val expect = "sucessful"
    assert(checkRec(input, expect, 206))
  }
  test("a mixed variable and array declaration list") {
    val input = "int a, b[5], c;"
    val expect = "sucessful"
    assert(checkRec(input, expect, 207))
  }
  test("a wrong variable declaration with initial value") {
    val input = "int a = 3;"
    val expect = "Error on line 1 col 7: ="
    assert(checkRec(input, expect, 208))
  }
  test("a wrong array declaration with wrong index") {
    val input = "boolean a[e];"
    val expect = "Error on line 1 col 11: e"
    assert(checkRec(input, expect, 209))
  }
  test("a wrong variable declaration with comma separator ") {
    val input = "string f, boolean a[e];"
    val expect = "Error on line 1 col 11: boolean"
    assert(checkRec(input, expect, 210))
  }
  test("a wrong variable declaration with semicolon separator") {
    val input = "float d; e;"
    val expect = "Error on line 1 col 10: e"
    assert(checkRec(input, expect, 211))
  }
  test("a function declaration without parameter") {
    val input = "void abc () {}"
    val expect = "sucessful"
    assert(checkRec(input, expect, 212))
  }
  test("a function declaration with one parameter") {
    val input = "void abc (int a) {}"
    val expect = "sucessful"
    assert(checkRec(input, expect, 213))
  }
  test("a function declaration with parameter list") {
    val input = "void abc (int a, string a[]) {}"
    val expect = "sucessful"
    assert(checkRec(input, expect, 214))
  }
  test("a function declaration with returned type is array pointer type") {
    val input = "int[] abc(){}"
    val expect = "sucessful"
    assert(checkRec(input, expect, 215))
  }
  test("a wrong function declaration missing brackets") {
    val input = "boolean abc {}"
    val expect = "Error on line 1 col 13: {"
    assert(checkRec(input, expect, 216))
  }
  test("a wrong function declaration missing parentheses") {
    val input = "string abc ()"
    val expect = "Error on line 1 col 14: <EOF>"
    assert(checkRec(input, expect, 217))
  }
  test("a wrong function declaration with semicolon at end") {
    val input = "float abc ();"
    val expect = "Error on line 1 col 13: ;"
    assert(checkRec(input, expect, 218))
  }
  test("a wrong function declaration with semicolon in parameter") {
    val input = "void abc (int a;) {}"
    val expect = "Error on line 1 col 16: ;"
    assert(checkRec(input, expect, 219))
  }
  test("a wrong function declaration with index in array parameter") {
    val input = "void abc (int a, string a[2]) {}"
    val expect = "Error on line 1 col 27: 2"
    assert(checkRec(input, expect, 220))
  }
  test("a wrong  function declaration with returned type is array pointer type having the size") {
    val input = "int[3] abc(){}"
    val expect = "Error on line 1 col 5: 3"
    assert(checkRec(input, expect, 221))
  }
  test("if statement without else case") {
    val input = """void main () {
if (a == b) a = d;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 222))
  }
  test("if statement with else case") {
    val input = """void main () {
if (a == b) a = d; else a = b;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 223))
  }
  test("if statement with many statements") {
    val input = """void main () {
if (a == b) a = d; a = b;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 224))
  }
  test("a wrong if statement due to missing brackets") {
    val input = """void main () {
if a==b a = d;
}
"""
    val expect = "Error on line 2 col 4: a"
    assert(checkRec(input, expect, 225))
  }
  test("a wrong if statement due to missing statement in else") {
    val input = """void main () {
if (a = b) a = d; else
}
"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input, expect, 226))
  }
  test("do while statement") {
    val input = """void main () {
do a = a + 1; while a < 10;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 227))
  }
  test("do while statement with many statements in do") {
    val input = """void main () {
do a = a + 1; b = a; while a < 10;
}"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 228))
  }
  test("do while statement with many statements after while") {
    val input = """void main () {
do a = a + 1; b = a; while a < 10; b > 9;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 229))
  }
  test("wrong do while statement no statement in do while") {
    val input = """void main () {
do while a < 10;
}
"""
    val expect = "Error on line 2 col 4: while"
    assert(checkRec(input, expect, 230))
  }
  test("wrong do while statement mising semicolon") {
    val input = """void main () {
do a = a + 1; while a < 10
}"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input, expect, 231))
  }
  test("wrong do while statement missing while expression") {
    val input = """void main () {
do a = a + 1;
}
"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input, expect, 232))
  }
  test("wrong do while statement mising do statement") {
    val input = """void main () {
while a < 10;
}"""
    val expect = "Error on line 2 col 1: while"
    assert(checkRec(input, expect, 233))
  }
  test("for statement") {
    val input = """void main () {
for (a = 1;  a < 10; a = a + 1) b = a;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 234))
  }
  test("wrong for statement due to using comma separator") {
    val input = """void main () {
for (a = 1,  a < 10, a = a + 1) b = a;
}
"""
    val expect = "Error on line 2 col 11: ,"
    assert(checkRec(input, expect, 235))
  }
  test("wrong for statement due to missing brackets") {
    val input = """void main () {
for a = 1;  a < 10; a = a + 1 b = a;
}
"""
    val expect = "Error on line 2 col 5: a"
    assert(checkRec(input, expect, 236))
  }
  test("wrong for statement due to missing statement in loop") {
    val input = """void main () {
for (a = 1;  a < 10; a = a + 1)
}
"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input, expect, 237))
  }
  test("wrong for statement due to missing expressions") {
    val input = """void main () {
for ( ; ; ) b = a;
}
"""
    val expect = "Error on line 2 col 7: ;"
    assert(checkRec(input, expect, 238))
  }
  test("wrong for statement due to missing expressions and seperator") {
    val input = """void main () {
for () b = a;
}
"""
    val expect = "Error on line 2 col 6: )"
    assert(checkRec(input, expect, 239))
  }
  test("break statement") {
    val input = """void main () {
break;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 240))
  }
  test("wrong break statement due to missing semicolon") {
    val input = """void main () {
break
}
"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input, expect, 241))
  }
  test("continue statement") {
    val input = """void main () {
continue;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 242))
  }
  test("wrong continue stateament due to missing semicolon") {
    val input = """void main () {
continue
}
"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input, expect, 243))
  }
  test("return statement having expression") {
    val input = """void main () {
return a < b;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 244))
  }
  test("return statement no expression") {
    val input = """void main () {
return;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 245))
  }
  test("wrong return statement having expression due to missing semicolon") {
    val input = """void main () {
return a < b
}
"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input, expect, 246))
  }
  test("wrong return statement no expression due to missing semicolon") {
    val input = """void main () {
return
}"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input, expect, 247))
  }
  test("expression statement with assign operator") {
    val input = """void main () {
a = 1;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 248))
  }
  test("expression statement with function call") {
    val input = """void main () {
foo (1,2);
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 249))
  }
  test("wrong expression statement with assign operator due to missing semicolon") {
    val input = """void main () {
a = a + 2
}
"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input, expect, 250))
  }
  test("wrong expression statement with function call due to missing semicolon") {
    val input = """void main () {
foo (1,2)
}
"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input, expect, 251))
  }
  test("block statement with variable declaration and statement") {
    val input = """void main () {
if (a == b) {
int d;
a = d = b + 3;
}
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 252))
  }
  test("block statement with variable declaration and without statement") {
    val input = """void main () {
if (a == b) {
int d;
}
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 253))
  }
  test("block statement without variable declaration but statement") {
    val input = """void main () {
if (a == b) {
a = d = b + 3;
}
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 254))
  }
  test("empty block statement") {
    val input = """void main () {
if (a == b) {
}
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 255))
  }
  test("wrong block statement due to variable declaration after statement") {
    val input = """void main () {
if (a == b) {
int d;
a = d = b + 3;
int fz;
}
}
"""
    val expect = "Error on line 5 col 1: int"
    assert(checkRec(input, expect, 256))
  }
  test("assign expression") {
    val input = """void main () {
a=3;
}"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 257))
  }
  test("many assign expresion") {
    val input = """void main () {
a=b=c=3;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 258))
  }
  test("wrong assign expression missing left hand side") {
    val input = """void main () {
= 3;
}
"""
    val expect = "Error on line 2 col 1: ="
    assert(checkRec(input, expect, 259))
  }
  test("wrong assign expression missing right hand side") {
    val input = """void main () {
a = ;
}
"""
    val expect = "Error on line 2 col 5: ;"
    assert(checkRec(input, expect, 260))
  }
  test("wrong assign expression missing assign operator") {
    val input = """void main () {
a 3;
}
"""
    val expect = "Error on line 2 col 3: 3"
    assert(checkRec(input, expect, 261))
  }
  test("OR expression") {
    val input = """void main () {
a = a || b;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 262))
  }
  test("Many OR expression") {
    val input = """void main () {
a = a || b || c;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 263))
  }
  test("wrong OR expression missing left hand side") {
    val input = """void main () {
a = || b;
}
"""
    val expect = "Error on line 2 col 5: ||"
    assert(checkRec(input, expect, 264))
  }
  test("wrong OR expression missing right hand side") {
    val input = """void main () {
a = a || ;
}
"""
    val expect = "Error on line 2 col 10: ;"
    assert(checkRec(input, expect, 265))
  }
  test("AND expression") {
    val input = """void main () {
a = a && b;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 266))
  }
  test("Many AND expression") {
    val input = """void main () {
a = a && b && c;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 267))
  }
  test("wrong AND expression missing left hand side") {
    val input = """void main () {
a = && b;
}
"""
    val expect = "Error on line 2 col 5: &&"
    assert(checkRec(input, expect, 268))
  }
  test("wrong AND expression missing right hand side") {
    val input = """void main () {
a = a && ;
}
"""
    val expect = "Error on line 2 col 10: ;"
    assert(checkRec(input, expect, 269))
  }
  test("Equal and not equal expression") {
    val input = """void main () {
a = a == b;
b = a != b;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 270))
  }
  test("wrong with many Equal expression") {
    val input = """void main () {
a = a == b == c;
a = a != b != c;
}
"""
    val expect = "Error on line 2 col 12: =="
    assert(checkRec(input, expect, 271))
  }
  test("wrong with many Not Equal expression") {
    val input = """void main () {
a = a == b;
a = a != b != c;
}
"""
    val expect = "Error on line 3 col 12: !="
    assert(checkRec(input, expect, 272))
  }
  test("wrong Equal and not equal expression missing left hand side") {
    val input = """void main () {
a =  == b;
}
"""
    val expect = "Error on line 2 col 6: =="
    assert(checkRec(input, expect, 273))
  }
  test("wrong Equal and not equal expression missing right hand side") {
    val input = """void main () {
b = a != ;
}
"""
    val expect = "Error on line 2 col 10: ;"
    assert(checkRec(input, expect, 274))
  }
  test("Comparison expression") {
    val input = """void main () {
a = a < b;
b = a > b;
a = a <= b;
b = a >= b;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 275))
  }
  test("wrong with many Comparison expression") {
    val input = """void main () {
a = a <= b > c;
}
"""
    val expect = "Error on line 2 col 12: >"
    assert(checkRec(input, expect, 276))
  }
  test("wrong comparison expression missing left hand side") {
    val input = """void main () {
a =  <= b;
}
"""
    val expect = "Error on line 2 col 6: <="
    assert(checkRec(input, expect, 277))
  }
  test("wrong comparison expression missing right hand side") {
    val input = """void main () {
b = a < ;
}
"""
    val expect = "Error on line 2 col 9: ;"
    assert(checkRec(input, expect, 278))
  }
  test("add, sub expression") {
    val input = """void main () {
a = a + b;
b = a - b;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 279))
  }
  test("many add, sub expression") {
    val input = """void main () {
a = a + b - c;
b = a - b + c;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 280))
  }
  test("wrong add expression missing left hand side") {
    val input = """void main () {
a = + c;
}
"""
    val expect = "Error on line 2 col 5: +"
    assert(checkRec(input, expect, 281))
  }
  test("wrong add, sub expression missing right hand side") {
    val input = """void main () {
a = a + ;
"""
    val expect = "Error on line 2 col 9: ;"
    assert(checkRec(input, expect, 282))
  }
  test("mul, div, mod expression") {
    val input = """void main () {
a = a * b;
b = a / b;
b = a % b;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 283))
  }
  test("many mul, div, mod expression") {
    val input = """void main () {
a = a * b / c - d;
b = a % b + c / d;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 284))
  }
  test("wrong mul, div, mod expression missing left hand side") {
    val input = """void main () {
a = / c;
}
"""
    val expect = "Error on line 2 col 5: /"
    assert(checkRec(input, expect, 285))
  }
  test("wrong mul, div, mod expression missing right hand side") {
    val input = """void main () {
a = a * ;
"""
    val expect = "Error on line 2 col 9: ;"
    assert(checkRec(input, expect, 286))
  }
  test("wrong binary expression missing mid operator") {
    val input = """void main () {
a = a 3;
}
"""
    val expect = "Error on line 2 col 7: 3"
    assert(checkRec(input, expect, 287))
  }
  test("Unary sub, not expression") {
    val input = """void main () {
a = -b;
b = !b;
}
"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 288))
  }
  test("wrong Unary sub, notexpression missing right operation") {
    val input = """void main () {
b = !;
}
"""
    val expect = "Error on line 2 col 6: ;"
    assert(checkRec(input, expect, 289))
  }
  test("wrong Unary not expression wrong order") {
    val input = """void main () {
a = b!;
}
"""
    val expect = "Error on line 2 col 6: !"
    assert(checkRec(input, expect, 290))
  }
  test("simple index operation") {
    val input = """void main () {
a=b[4];
}"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 291))
  }
  test("more index operation") {
    val input = """void main () {
a=f(d)[4 + v];
}"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 292))
  }
  test("wrong index operation missing first expression before []") {
    val input = """void main () {
a=[4 + v];
}"""
    val expect = "Error on line 2 col 3: ["
    assert(checkRec(input, expect, 293))
  }
  test("wrong index operation missing second expression between []") {
    val input = """void main () {
a=f(d)[];
}"""
    val expect = "Error on line 2 col 8: ]"
    assert(checkRec(input, expect, 294))
  }
  test("function call without input") {
    val input = """void main () {
a=f();
}"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 295))
  }
  test("function call with one input") {
    val input = """void main () {
a=f(sdf);
}"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 296))
  }
  test("function call with many input") {
    val input = """void main () {
a=f(sdf, pdf[wr], 23);
}"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 297))
  }
  test("wrong function call with one input having comma") {
    val input = """void main () {
a=f(s,);
}"""
    val expect = "Error on line 2 col 7: )"
    assert(checkRec(input, expect, 298))
  }
  test("wrong function call with many input that using semiconlon seperator") {
    val input = """void main () {
a=f(s; pdf[wr]; 23);
}"""
    val expect = "Error on line 2 col 6: ;"
    assert(checkRec(input, expect, 299))
  }
  test("Using brackets in expression") {
    val input = """void main () {
a= (a + b) * d - (c + d);
}"""
    val expect = "sucessful"
    assert(checkRec(input, expect, 300))
  }

}