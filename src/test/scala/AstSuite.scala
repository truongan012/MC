import org.scalatest.FunSuite
import mc.utils._

/**
  * Created by nhphung on 4/29/17.
  */
class AstSuite extends FunSuite with TestAst {
  test("a simple program") {
    val input = "int main () {}"
    val expect = Program(List(FuncDecl(Id("main"),List(),IntType,Block(List(),List()))))
    assert(checkAst(input, expect, 301))
  }
  test("more complex program") {
    val input = """void main () {
 putIntLn(4);
}"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(CallExpr(Id("putIntLn"),List(IntLiteral(4))))))))
    assert(checkAst(input, expect, 302))
  }
  test("a simple variable declaration") {
    val input = "int a;"
    val expect = Program(List(VarDecl(Id("a"),IntType)))
    assert(checkAst(input, expect, 303))
  }
  test("a array variable declaration") {
    val input = "boolean a[3];"
    val expect = Program(List(VarDecl(Id("a"),ArrayType(IntLiteral(3), BoolType))))
    assert(checkAst(input, expect, 304))
  }
  test("a simple variable declaration list") {
    val input = "float a, b;"
    val expect = Program(List(VarDecl(Id("a"),FloatType),VarDecl(Id("b"),FloatType)))
    assert(checkAst(input, expect, 305))
  }
  test("a mixed variable and array declaration list") {
    val input = "int a, b[5], c;"
    val expect = Program(List(VarDecl(Id("a"),IntType),VarDecl(Id("b"),ArrayType(IntLiteral(5),IntType)),VarDecl(Id("c"),IntType)))
    assert(checkAst(input, expect, 306))
  }
  test("a function declaration without parameter") {
    val input = "void abc () {}"
    val expect = Program(List(FuncDecl(Id("abc"),List(),VoidType,Block(List(),List()))))
    assert(checkAst(input, expect, 307))
  }
  test("a function declaration with one parameter") {
    val input = "void abc (int a) {}"
    val expect = Program(List(FuncDecl(Id("abc"),List(VarDecl(Id("a"),IntType)),VoidType,Block(List(),List()))))
    assert(checkAst(input, expect, 308))
  }
  test("a function declaration with parameter list") {
    val input = "void abc (int a, string a[]) {}"
    val expect = Program(List(FuncDecl(Id("abc"),List(VarDecl(Id("a"),IntType),VarDecl(Id("a"),ArrayPointerType(StringType))),VoidType,Block(List(),List()))))
    assert(checkAst(input, expect, 309))
  }
  test("a function declaration with returned type is array pointer type") {
    val input = "int[] abc(){}"
    val expect = Program(List(FuncDecl(Id("abc"),List(),ArrayPointerType(IntType),Block(List(),List()))))
    assert(checkAst(input, expect, 310))
  }
  test("a function declaration with returned type is array pointer type and parameter list") {
    val input = "int[] abc(int a, boolean cd[]) {}"
    val expect = Program(List(FuncDecl(Id("abc"),List(VarDecl(Id("a"),IntType),VarDecl(Id("cd"),ArrayPointerType(BoolType))),ArrayPointerType(IntType),Block(List(),List()))))
    assert(checkAst(input, expect, 311))
  }
  test("if statement without else case") {
    val input = """void main () {
if (a == b) a = d;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(If(BinaryOp("==",Id("a"),Id("b")),BinaryOp("=",Id("a"),Id("d")),None))))))
    assert(checkAst(input, expect, 312))
  }
  test("if statement with else case") {
    val input = """void main () {
if (a == b) a = d; else a = b;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(If(BinaryOp("==",Id("a"),Id("b")),BinaryOp("=",Id("a"),Id("d")),Some(BinaryOp("=",Id("a"),Id("b")))))))))
    assert(checkAst(input, expect, 313))
  }
  test("if statement with many statements") {
    val input = """void main () {
if (a == b) a = d; a = b;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(If(BinaryOp("==",Id("a"),Id("b")),BinaryOp("=",Id("a"),Id("d")),None),BinaryOp("=",Id("a"),Id("b")))))))
    assert(checkAst(input, expect, 314))
  }
  test("do while statement") {
    val input = """void main () {
do a = a + 1; while a < 10;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(Dowhile(List(BinaryOp("=",Id("a"),BinaryOp("+",Id("a"),IntLiteral(1)))),BinaryOp("<",Id("a"),IntLiteral(10))))))))
    assert(checkAst(input, expect, 315))
  }
  test("do while statement with many statements in do") {
    val input = """void main () {
do a = a + 1; b = a; while a < 10;
}"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(Dowhile(List(BinaryOp("=",Id("a"),BinaryOp("+",Id("a"),IntLiteral(1))),BinaryOp("=",Id("b"),Id("a"))),BinaryOp("<",Id("a"),IntLiteral(10))))))))
    assert(checkAst(input, expect, 316))
  }
  test("do while statement with many statements after while") {
    val input = """void main () {
do a = a + 1; b = a; while a < 10; b > 9;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(Dowhile(List(BinaryOp("=",Id("a"),BinaryOp("+",Id("a"),IntLiteral(1))),BinaryOp("=",Id("b"),Id("a"))),BinaryOp("<",Id("a"),IntLiteral(10))),BinaryOp(">",Id("b"),IntLiteral(9)))))))
    assert(checkAst(input, expect, 317))
  }
  test("for statement") {
    val input = """void main () {
for (a = 1;  a < 10; a = a + 1) b = a;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(For(BinaryOp("=",Id("a"),IntLiteral(1)),BinaryOp("<",Id("a"),IntLiteral(10)),BinaryOp("=",Id("a"),BinaryOp("+",Id("a"),IntLiteral(1))),BinaryOp("=",Id("b"),Id("a"))))))))
    assert(checkAst(input, expect, 318))
  }
  test("break statement") {
    val input = """void main () {
break;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(Break)))))
    assert(checkAst(input, expect, 319))
  }
  test("continue statement") {
    val input = """void main () {
continue;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(Continue)))))
    assert(checkAst(input, expect, 320))
  }
  test("return statement having expression") {
    val input = """void main () {
return a < b;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(Return(Some(BinaryOp("<",Id("a"),Id("b")))))))))
    assert(checkAst(input, expect, 321))
  }
  test("return statement no expression") {
    val input = """void main () {
return;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(Return(None))))))
    assert(checkAst(input, expect, 322))
  }
  test("expression statement with assign operator") {
    val input = """void main () {
a = 1;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),IntLiteral(1)))))))
    assert(checkAst(input, expect, 323))
  }
  test("expression statement with function call") {
    val input = """void main () {
foo (1,2);
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(CallExpr(Id("foo"),List(IntLiteral(1),IntLiteral(2))))))))
    assert(checkAst(input, expect, 324))
  }
  test("expression statement with function call papramter expression") {
    val input = """void main () {
foo ( a + 1, bcd);
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(CallExpr(Id("foo"),List(BinaryOp("+",Id("a"),IntLiteral(1)),Id("bcd"))))))))
    assert(checkAst(input, expect, 325))
  }
  test("expression statement with function call, complex paramter expression") {
    val input = """void main () {
foo ( B < 30, (anv + 3) < (bc - 2.0e7));
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(CallExpr(Id("foo"),List(BinaryOp("<",Id("B"),IntLiteral(30)),BinaryOp("<",BinaryOp("+",Id("anv"),IntLiteral(3)),BinaryOp("-",Id("bc"),FloatLiteral(2.0E7f))))))))))
    assert(checkAst(input, expect, 326))
  }
  test("block statement with variable declaration and statement") {
    val input = """void main () {
if (a == b) {
int d;
a = d = b + 3;
}
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(If(BinaryOp("==",Id("a"),Id("b")),Block(List(VarDecl(Id("d"),IntType)),List(BinaryOp("=",Id("a"),BinaryOp("=",Id("d"),BinaryOp("+",Id("b"),IntLiteral(3)))))),None))))))
    assert(checkAst(input, expect, 327))
  }
  test("block statement with variable declaration and without statement") {
    val input = """void main () {
if (a == b) {
int d;
}
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(If(BinaryOp("==",Id("a"),Id("b")),Block(List(VarDecl(Id("d"),IntType)),List()),None))))))
    assert(checkAst(input, expect, 328))
  }
  test("block statement without variable declaration but statement") {
    val input = """void main () {
if (a == b) {
a = d = b + 3;
}
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(If(BinaryOp("==",Id("a"),Id("b")),Block(List(),List(BinaryOp("=",Id("a"),BinaryOp("=",Id("d"),BinaryOp("+",Id("b"),IntLiteral(3)))))),None))))))
    assert(checkAst(input, expect, 329))
  }
  test("empty block statement") {
    val input = """void main () {
if (a == b) {
}
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(If(BinaryOp("==",Id("a"),Id("b")),Block(List(),List()),None))))))
    assert(checkAst(input, expect, 330))
  }
  test("assign expression") {
    val input = """void main () {
a=foo(1, 2);
}"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),CallExpr(Id("foo"),List(IntLiteral(1),IntLiteral(2)))))))))
    assert(checkAst(input, expect, 331))
  }
  test("many assign expresion") {
    val input = """void main () {
a=b=c=3;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp("=",Id("b"),BinaryOp("=",Id("c"),IntLiteral(3)))))))))
    assert(checkAst(input, expect, 332))
  }
  test("OR expression") {
    val input = """void main () {
a = a || b;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp("||",Id("a"),Id("b"))))))))
    assert(checkAst(input, expect, 333))
  }
  test("Many OR expression") {
    val input = """void main () {
a = a || b || c;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp("||",BinaryOp("||",Id("a"),Id("b")),Id("c"))))))))
    assert(checkAst(input, expect, 334))
  }
  test("AND expression") {
    val input = """void main () {
a = a && b;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp("&&",Id("a"),Id("b"))))))))
    assert(checkAst(input, expect, 335))
  }
  test("Many AND expression") {
    val input = """void main () {
a = a && b && c;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp("&&",BinaryOp("&&",Id("a"),Id("b")),Id("c"))))))))
    assert(checkAst(input, expect, 336))
  }
  test("Equal and not equal expression") {
    val input = """void main () {
a = a == b;
b = a != b;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp("==",Id("a"),Id("b"))),BinaryOp("=",Id("b"),BinaryOp("!=",Id("a"),Id("b"))))))))
    assert(checkAst(input, expect, 337))
  }
  test("Less than comparison expression") {
    val input = """void main () {
a = a < b;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp("<",Id("a"),Id("b"))))))))
    assert(checkAst(input, expect, 338))
  }
  test("Greater than comparison expression") {
    val input = """void main () {
a = a > b;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp(">",Id("a"),Id("b"))))))))
    assert(checkAst(input, expect, 339))
  }
  test("Less than or equal comparison expression") {
    val input = """void main () {
a = a <= b;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp("<=",Id("a"),Id("b"))))))))
    assert(checkAst(input, expect, 340))
  }
  test("Greater than or equal comparison expression") {
    val input = """void main () {
a = a >= b;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp(">=",Id("a"),Id("b"))))))))
    assert(checkAst(input, expect, 341))
  }
  test("mul expression") {
    val input = """void main () {
a = a * b;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp("*",Id("a"),Id("b"))))))))
    assert(checkAst(input, expect, 342))
  }
  test("div expression") {
    val input = """void main () {
b = a / b;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("b"),BinaryOp("/",Id("a"),Id("b"))))))))
    assert(checkAst(input, expect, 343))
  }
  test("mod expression") {
    val input = """void main () {
b = a % b;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("b"),BinaryOp("%",Id("a"),Id("b"))))))))
    assert(checkAst(input, expect, 344))
  }
  test("many mul expression") {
    val input = """void main () {
a = a * b * c;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp("*",BinaryOp("*",Id("a"),Id("b")),Id("c"))))))))
    assert(checkAst(input, expect, 345))
  }
  test("many div expression") {
    val input = """void main () {
a = a / b / c;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp("/",BinaryOp("/",Id("a"),Id("b")),Id("c"))))))))
    assert(checkAst(input, expect, 346))
  }
  test("many mod expression") {
    val input = """void main () {
a = a % b % c;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp("%",BinaryOp("%",Id("a"),Id("b")),Id("c"))))))))
    assert(checkAst(input, expect, 347))
  }
  test("Unary not expression") {
    val input = """void main () {
b = !b;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("b"),UnaryOp("!",Id("b"))))))))
    assert(checkAst(input, expect, 348))
  }
  test("Unary sub expression") {
    val input = """void main () {
a = -b;
}
"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),UnaryOp("-",Id("b"))))))))
    assert(checkAst(input, expect, 349))
  }
  test("simple index operation") {
    val input = """void main () {
a=b[4];
}"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),ArrayCell(Id("b"),IntLiteral(4))))))))
    assert(checkAst(input, expect, 350))
  }
  test("more index operation") {
    val input = """void main () {
a=f(d)[4 + v];
}"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),ArrayCell(CallExpr(Id("f"),List(Id("d"))),BinaryOp("+",IntLiteral(4),Id("v")))))))))
    assert(checkAst(input, expect, 351))
  }
  test("function call without input") {
    val input = """void main () {
a=f();
}"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),CallExpr(Id("f"),List())))))))
    assert(checkAst(input, expect, 352))
  }
  test("function call with one input") {
    val input = """void main () {
a=f(sdf);
}"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),CallExpr(Id("f"),List(Id("sdf")))))))))
    assert(checkAst(input, expect, 353))
  }
  test("function call with many input") {
    val input = """void main () {
a=f(sdf, pdf[wr], 23);
}"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),CallExpr(Id("f"),List(Id("sdf"),ArrayCell(Id("pdf"),Id("wr")),IntLiteral(23)))))))))
    assert(checkAst(input, expect, 354))
  }
  test("Using brackets in expression") {
    val input = """void main () {
a= (a + b) * d;
}"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp("*",BinaryOp("+",Id("a"),Id("b")),Id("d"))))))))
    assert(checkAst(input, expect, 355))
  }
  test("Using brackets in complex expression") {
    val input = """void main () {
a= (a + b) * d - (c + d);
}"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BinaryOp("-",BinaryOp("*",BinaryOp("+",Id("a"),Id("b")),Id("d")),BinaryOp("+",Id("c"),Id("d")))))))))
    assert(checkAst(input, expect, 356))
  }
  test("variable declaration and function declaration without parameter") {
    val input = """int a, b;
void abc () {}
void abcs () {}"""
    val expect = Program(List(VarDecl(Id("a"),IntType),VarDecl(Id("b"),IntType),FuncDecl(Id("abc"),List(),VoidType,Block(List(),List())),FuncDecl(Id("abcs"),List(),VoidType,Block(List(),List()))))
    assert(checkAst(input, expect, 357))
  }
  test("variable declaration and function declaration with one parameter") {
    val input = """int a, b;
void abc (int a) {}
int c;"""
    val expect = Program(List(VarDecl(Id("a"),IntType),VarDecl(Id("b"),IntType),FuncDecl(Id("abc"),List(VarDecl(Id("a"),IntType)),VoidType,Block(List(),List())),VarDecl(Id("c"),IntType)))
    assert(checkAst(input, expect, 358))
  }
  test("variable declaration and function declaration with parameter list") {
    val input = """int a, b;
int d;
void abc (int a, string a[]) {}"""
    val expect = Program(List(VarDecl(Id("a"),IntType),VarDecl(Id("b"),IntType),VarDecl(Id("d"),IntType),FuncDecl(Id("abc"),List(VarDecl(Id("a"),IntType),VarDecl(Id("a"),ArrayPointerType(StringType))),VoidType,Block(List(),List()))))
    assert(checkAst(input, expect, 359))
  }
  test("variable declaration and function declaration with returned type is array pointer type") {
    val input = """int a, b;
boolean e, f;
int[] abc(){}
void main(){}"""
    val expect = Program(List(VarDecl(Id("a"),IntType),VarDecl(Id("b"),IntType),VarDecl(Id("e"),BoolType),VarDecl(Id("f"),BoolType),FuncDecl(Id("abc"),List(),ArrayPointerType(IntType),Block(List(),List())),FuncDecl(Id("main"),List(),VoidType,Block(List(),List()))))
    assert(checkAst(input, expect, 360))
  }
  test("A sample program") {
    val input = """int a, b;
int main () {
a=3;
putIntLn(a);
}"""
    val expect = Program(List(VarDecl(Id("a"),IntType),VarDecl(Id("b"),IntType),FuncDecl(Id("main"),List(),IntType,Block(List(),List(BinaryOp("=",Id("a"),IntLiteral(3)),CallExpr(Id("putIntLn"),List(Id("a"))))))))
    assert(checkAst(input, expect, 361))
  }
  test("A more complex program") {
    val input = """int a, b;
int main () {
a=3;
putIntLn(a);
}"""
    val expect = Program(List(VarDecl(Id("a"),IntType),VarDecl(Id("b"),IntType),FuncDecl(Id("main"),List(),IntType,Block(List(),List(BinaryOp("=",Id("a"),IntLiteral(3)),CallExpr(Id("putIntLn"),List(Id("a"))))))))
    assert(checkAst(input, expect, 362))
  }
  test("A more complex program with statements 1") {
    val input = """int a, b;
int main () {
a=3;
if (a == b) putIntLn(a);
}"""
    val expect = Program(List(VarDecl(Id("a"),IntType),VarDecl(Id("b"),IntType),FuncDecl(Id("main"),List(),IntType,Block(List(),List(BinaryOp("=",Id("a"),IntLiteral(3)),If(BinaryOp("==",Id("a"),Id("b")),CallExpr(Id("putIntLn"),List(Id("a"))),None))))))
    assert(checkAst(input, expect, 363))
  }
  test("A more complex program with statements 2") {
    val input = """int a, b;
void main () {
float c;
c = 2.0e-3;
a = 1;
do a = a + 1; b = a / 2; c = c + b;
while a < 10;
}
"""
    val expect = Program(List(VarDecl(Id("a"),IntType),VarDecl(Id("b"),IntType),FuncDecl(Id("main"),List(),VoidType,Block(List(VarDecl(Id("c"),FloatType)),List(BinaryOp("=",Id("c"),FloatLiteral(0.002f)),BinaryOp("=",Id("a"),IntLiteral(1)),Dowhile(List(BinaryOp("=",Id("a"),BinaryOp("+",Id("a"),IntLiteral(1))),BinaryOp("=",Id("b"),BinaryOp("/",Id("a"),IntLiteral(2))),BinaryOp("=",Id("c"),BinaryOp("+",Id("c"),Id("b")))),BinaryOp("<",Id("a"),IntLiteral(10))))))))
    assert(checkAst(input, expect, 364))
  }
  test("Assign expression with string literal") {
    val input = """void main () {
a="This is string";
}"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),StringLiteral("This is string")))))))
    assert(checkAst(input, expect, 365))
  }
  test("Assign expression with bolean literal") {
    val input = """void main () {
a=true;
}"""
    val expect = Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List(BinaryOp("=",Id("a"),BooleanLiteral(true)))))))
    assert(checkAst(input, expect, 366))
  }

}