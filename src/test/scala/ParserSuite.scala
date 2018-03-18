import org.scalatest.FunSuite

/**
  * Created by nhphung on 4/28/17.
  */
class ParserSuite  extends FunSuite with TestParser {

  test("a simple program") {
    val input = "int main () {}"
    val expect = "sucessful"
    assert(checkRec(input,expect,201))
  }
  test("more complex program") {
    val input = """int main () {
 putIntLn(4);
}"""
    val expect = "sucessful"
    assert(checkRec(input,expect,202))
  }
  test("wrong program") {
    val input = "} int main {"
    val expect = "Error on line 1 col 1: }"
    assert(checkRec(input,expect,203))
  }
  test("a simple variable declaration 1") {
    val input = "int a;"
    val expect = "sucessful"
    assert(checkRec(input,expect,204))
  }
  test("a simple variable declaration 2") {
    val input = "int a[3];"
    val expect = "sucessful"
    assert(checkRec(input,expect,205))
  }
  test("a simple variable declaration list 1") {
    val input = "int a, b;"
    val expect = "sucessful"
    assert(checkRec(input,expect,206))
  }
  test("a simple variable declaration list 2") {
    val input = "int a, b[5], c;"
    val expect = "sucessful"
    assert(checkRec(input,expect,207))
  }
  test("a wrong variable declaration 1") {
    val input = "int a = 3;"
    val expect = "Error on line 1 col 7: ="
    assert(checkRec(input,expect,208))
  }
  test("a wrong variable declaration 2") {
    val input = "boolean a[e];"
    val expect = "Error on line 1 col 11: e"
    assert(checkRec(input,expect,209))
  }
  test("a wrong variable declaration 3") {
    val input = "string f, boolean a[e];"
    val expect = "Error on line 1 col 11: boolean"
    assert(checkRec(input,expect,210))
  }
  test("a wrong variable declaration 4") {
    val input = "float d; e;"
    val expect = "Error on line 1 col 10: e"
    assert(checkRec(input,expect,211))
  }
  test("a function declaration 1") {
    val input = "void abc () {}"
    val expect = "sucessful"
    assert(checkRec(input,expect,212))
  }
  test("a function declaration 2") {
    val input = "void abc (int a) {}"
    val expect = "sucessful"
    assert(checkRec(input,expect,213))
  }
  test("a function declaration 3") {
    val input = "void abc (int a, string a[]) {}"
    val expect = "sucessful"
    assert(checkRec(input,expect,214))
  }
  test("a wrong function declaration 1") {
    val input = "boolean abc {}"
    val expect = "Error on line 1 col 13: {"
    assert(checkRec(input,expect,215))
  }
  test("a wrong function declaration 2") {
    val input = "string abc ()"
    val expect = "Error on line 1 col 14: <EOF>"
    assert(checkRec(input,expect,216))
  }
  test("a wrong function declaration 3") {
    val input = "float abc ();"
    val expect = "Error on line 1 col 13: ;"
    assert(checkRec(input,expect,217))
  }
  test("a wrong function declaration 4") {
    val input = "void abc (int a;) {}"
    val expect = "Error on line 1 col 16: ;"
    assert(checkRec(input,expect,218))
  }
  test("a wrong function declaration 5") {
    val input = "void abc (int a, string a[2]) {}"
    val expect = "Error on line 1 col 27: 2"
    assert(checkRec(input,expect,219))
  }
  test("if statement without else case") {
    val input = """void main () {
if (a == b) a = d;
}"""
    val expect = "sucessful"
    assert(checkRec(input,expect,220))
  }
  test("if statement with else case") {
    val input = """void main () {
if (a == b) a = d; else a = b;
}"""
    val expect = "sucessful"
    assert(checkRec(input,expect,221))
  }
  test("if statement") {
    val input = """void main () {
if (a == b) a = d; a = b;
}"""
    val expect = "sucessful"
    assert(checkRec(input,expect,222))
  }
  test("a wrong if statement 1") {
    val input = """void main () {
if a==b a = d;
}"""
    val expect = "Error on line 2 col 4: a"
    assert(checkRec(input,expect,223))
  }
  test("a wrong if statement 2") {
    val input = """void main () {
if (a = b) a = d; else
}"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input,expect,224))
  }
  test("do while statement 1") {
    val input = """void main () {
do a = a + 1; while a < 10;
}"""
    val expect = "sucessful"
    assert(checkRec(input,expect,225))
  }
  test("do while statement 2") {
    val input = """void main () {
do a = a + 1; b = a; while a < 10;
}"""
    val expect = "sucessful"
    assert(checkRec(input,expect,226))
  }
  test("do while statement 3") {
    val input = """void main () {
do a = a + 1; b = a; while a < 10; b > 9;
}"""
    val expect = "sucessful"
    assert(checkRec(input,expect,227))
  }
  test("wrong do while statement 1") {
    val input = """void main () {
do while a < 10;
}"""
    val expect = "Error on line 2 col 4: while"
    assert(checkRec(input,expect,228))
  }
  test("wrong do while statement 2") {
    val input = """void main () {
do a = a + 1; while a < 10
}"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input,expect,229))
  }
  test("wrong do while statement 3") {
    val input = """void main () {
do a = a + 1;
}"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input,expect,230))
  }
  test("wrong do while statement 4") {
    val input = """void main () {
while a < 10;
}"""
    val expect = "Error on line 2 col 1: while"
    assert(checkRec(input,expect,231))
  }
  test("for statement 1") {
    val input = """void main () {
for (a = 1;  a < 10; a = a + 1) b = a;
}"""
    val expect = "sucessful"
    assert(checkRec(input,expect,232))
  }
  test("wrong for statement 1") {
    val input = """void main () {
for (a = 1,  a < 10, a = a + 1) b = a;
}"""
    val expect = "Error on line 2 col 11: ,"
    assert(checkRec(input,expect,233))
  }
  test("wrong for statement 2") {
    val input = """void main () {
for a = 1;  a < 10; a = a + 1 b = a;
}"""
    val expect = "Error on line 2 col 5: a"
    assert(checkRec(input,expect,234))
  }
  test("wrong for statement 3") {
    val input = """void main () {
for (a = 1;  a < 10; a = a + 1)
}"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input,expect,235))
  }
  test("wrong for statement 4") {
    val input = """void main () {
for ( ; ; ) b = a;
}"""
    val expect = "Error on line 2 col 7: ;"
    assert(checkRec(input,expect,236))
  }
  test("wrong for statement 5") {
    val input = """void main () {
for () b = a;
}"""
    val expect = "Error on line 2 col 6: )"
    assert(checkRec(input,expect,237))
  }
  test("break statement") {
    val input = """void main () {
break;
}"""
    val expect = "sucessful"
    assert(checkRec(input,expect,238))
  }
  test("wrong break statement") {
    val input = """void main () {
break
}"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input,expect,239))
  }
  test("continue statement") {
    val input = """void main () {
continue;
}"""
    val expect = "sucessful"
    assert(checkRec(input,expect,240))
  }
  test("wrong continue stateament") {
    val input = """void main () {
continue
}"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input,expect,241))
  }
  test("return statement 1") {
    val input = """void main () {
return a < b;
}"""
    val expect = "sucessful"
    assert(checkRec(input,expect,242))
  }
  test("return statement 2") {
    val input = """void main () {
return;
}"""
    val expect = "sucessful"
    assert(checkRec(input,expect,243))
  }
  test("wrong return statement 1") {
    val input = """void main () {
return a < b
}"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input,expect,244))
  }
  test("wrong return statement 2") {
    val input = """void main () {
return
}"""
    val expect = "Error on line 3 col 1: }"
    assert(checkRec(input,expect,245))
  }

}