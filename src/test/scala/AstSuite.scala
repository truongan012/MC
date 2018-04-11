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
    val input = """int main () {
 putIntLn(4);
}"""
    val expect = Program(List(FuncDecl(Id("main"),List(),IntType,Block(List(),List(CallExpr(Id("putIntLn"),List(IntLiteral(4))))))))
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

}