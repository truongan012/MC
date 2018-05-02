import org.scalatest.FunSuite
import mc.checker._
import mc.utils._

/**
  * Created by nhphung on 4/29/17.
  */
class CheckerSuite extends FunSuite with TestChecker {
  test("No Entry Point: Wrong name") {
    val input =
      """int a, b;
        |float c[10];
        |void Main(){}
      """.stripMargin
    val expect = NoEntryPoint.getMessage
    assert(checkCkr(input, expect, 1))
  }
  test("No Entry Point: Wrong type") {
    val input =
      """int a, b;
        |float c[10];
        |int main(){}
      """.stripMargin
    val expect = NoEntryPoint.getMessage
    assert(checkCkr(input, expect, 2))
  }
  test("No Entry Point: Having parameter") {
    val input =
      """int a, b;
        |float c[10];
        |void main(int f){}
      """.stripMargin
    val expect = NoEntryPoint.getMessage
    assert(checkCkr(input, expect, 3))
  }
  test("Redeclared Variable: Global Environment") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){}
        |int c;
      """.stripMargin
    val expect = Redeclared(Variable, "c").getMessage
    assert(checkCkr(input, expect, 4))
  }
  test("Redeclared Function with Variable: Global Environment") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){}
        |int a(){}
      """.stripMargin
    val expect = Redeclared(Function, "a").getMessage
    assert(checkCkr(input, expect, 5))
  }
  test("Redeclared Function with Function: Global Environment") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){}
        |int a(){}
      """.stripMargin
    val expect = Redeclared(Function, "a").getMessage
    assert(checkCkr(input, expect, 6))
  }
  test("Redeclared Variable: Local Environment") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){}
        |int test(int a[], boolean b, string c){
        |  float c[5], d;
        |  return d;
        |}
      """.stripMargin
    val expect = Redeclared(Variable, "c").getMessage
    assert(checkCkr(input, expect, 7))
  }
  test("Redeclared Parameter") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){}
        |int test(int a[], boolean d, string a){
        |  float c[5];
        |  return b;
        |}
      """.stripMargin
    val expect = Redeclared(Parameter, "a").getMessage
    assert(checkCkr(input, expect, 8))
  }
  test("Undeclared Function") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){}
        |int test(int a[], boolean d, string s){
        |  b = foo();
        |  return b;
        |}
      """.stripMargin
    val expect = Undeclared(Function, "foo").getMessage
    assert(checkCkr(input, expect, 9))
  }
  test("Undeclared Identifier") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){}
        |int test(int a[], boolean b, string s){
        |  float c[5];
        |  return d;
        |}
      """.stripMargin
    val expect = Undeclared(Identifier, "d").getMessage
    assert(checkCkr(input, expect, 10))
  }
  test("Type Mismatch In Statement If") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){
        |  if(a) b = 10;
        |}
        |int test(){
        |  return 5;
        |}
      """.stripMargin
    val expect = TypeMismatchInStatement(If(Id("a"), BinaryOp("=", Id("b"), IntLiteral(10)), None)).getMessage
    assert(checkCkr(input, expect, 11))
  }
  test("Type Mismatch In Statement For: Expression 1") {
    val input =
      """int a, b;
        |void main(){
        | boolean b;
        |  for(b; a > 5; a + 1) {
        |  int d;
        |  d = a + test();
        |  }
        |}
        |int test(){
        |  return 5;
        |}
      """.stripMargin
    val expect = TypeMismatchInStatement(For(Id("b"), BinaryOp(">", Id("a"), IntLiteral(5)), BinaryOp("+", Id("a"), IntLiteral(1)), Block(List(VarDecl(Id("d"), IntType)), List(BinaryOp("=", Id("d"), BinaryOp("+", Id("a"), CallExpr(Id("test"), List()))))))).getMessage
    assert(checkCkr(input, expect, 12))
  }
  test("Type Mismatch In Statement For: Expression 2") {
    val input =
      """int a, b;
        |void main(){
        | boolean b;
        |  for(a; a + 5; a + 1) {
        |  int d;
        |  d = a + test();
        |  }
        |}
        |int test(){
        |  return 5;
        |}
      """.stripMargin
    val expect = TypeMismatchInStatement(For(Id("a"), BinaryOp("+", Id("a"), IntLiteral(5)), BinaryOp("+", Id("a"), IntLiteral(1)), Block(List(VarDecl(Id("d"), IntType)), List(BinaryOp("=", Id("d"), BinaryOp("+", Id("a"), CallExpr(Id("test"), List()))))))).getMessage
    assert(checkCkr(input, expect, 13))
  }
  test("Type Mismatch In Statement For: Expression 3") {
    val input =
      """int a, b;
        |void main(){
        | boolean b;
        |  for(a; a < 5; b) {
        |  int d;
        |  d = a + test();
        |  }
        |}
        |int test(){
        |  return 5;
        |}
      """.stripMargin
    val expect = TypeMismatchInStatement(For(Id("a"), BinaryOp("<", Id("a"), IntLiteral(5)), Id("b"), Block(List(VarDecl(Id("d"), IntType)), List(BinaryOp("=", Id("d"), BinaryOp("+", Id("a"), CallExpr(Id("test"), List()))))))).getMessage
    assert(checkCkr(input, expect, 14))
  }
  test("Type Mismatch In Statement DoWhile") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){
        |  do
        |  a = a + 1;
        |  while a % test();
        |}
        |int test(){
        |  return 5;
        |}
      """.stripMargin
    val expect = TypeMismatchInStatement(Dowhile(List(BinaryOp("=", Id("a"), BinaryOp("+", Id("a"), IntLiteral(1)))), BinaryOp("%", Id("a"), CallExpr(Id("test"), List())))).getMessage
    assert(checkCkr(input, expect, 15))
  }
  test("Type Mismatch In Statement Return Void") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){
        |  do
        |  a = a + 1;
        |  while a == 10;
        |}
        |void test(){
        |  return 5;
        |}
      """.stripMargin
    val expect = TypeMismatchInStatement(Return(Some(IntLiteral(5)))).getMessage
    assert(checkCkr(input, expect, 16))
  }
  test("Type Mismatch In Statement Return not Void") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){
        |  do
        |  a = a + 1;
        |  while a == test();
        |}
        |int test(){
        |  return 5.0;
        |}
      """.stripMargin
    val expect = TypeMismatchInStatement(Return(Some(FloatLiteral(5.0f)))).getMessage
    assert(checkCkr(input, expect, 17))
  }
  test("Type Mismatch In Expression: array subcripting E1") {
    val input =
      """int a, b;
        |float c[10];
        |float d;
        |void main(){
        | d = a[5];
        |}
      """.stripMargin
    val expect = TypeMismatchInExpression(ArrayCell(Id("a"), IntLiteral(5))).getMessage
    assert(checkCkr(input, expect, 18))
  }
  test("Type Mismatch In Expression: array subcripting E2") {
    val input =
      """int a, b;
        |float c[10];
        |float d;
        |void main(){
        | d = c[d];
        |}
      """.stripMargin
    val expect = TypeMismatchInExpression(ArrayCell(Id("c"), Id("d"))).getMessage
    assert(checkCkr(input, expect, 19))
  }
  test("Type Mismatch In Expression: Binary Op") {
    val input =
      """int a, b;
        |float c[10];
        |float d;
        |void main(){
        | a = a + true;
        | a = d + b;
        |}
      """.stripMargin
    val expect = TypeMismatchInExpression(BinaryOp("+", Id("a"), BooleanLiteral(true))).getMessage
    assert(checkCkr(input, expect, 20))
  }
  test("Type Mismatch In Expression: Unary Op") {
    val input =
      """int a, b;
        |float c[10];
        |float d;
        |void main(){
        | d = -a;
        | d = !a;
        |}
      """.stripMargin
    val expect = TypeMismatchInExpression(UnaryOp("!", Id("a"))).getMessage
    assert(checkCkr(input, expect, 21))
  }
  test("Type Mismatch In Expression: Assignment 1") {
    val input =
      """int a, b;
        |float c[10];
        |float d;
        |void main(){
        | a = d;
        |}
      """.stripMargin
    val expect = TypeMismatchInExpression(BinaryOp("=", Id("a"), Id("d"))).getMessage
    assert(checkCkr(input, expect, 22))
  }
  test("Type Mismatch In Expression: Assignment 2") {
    val input =
      """int a, b;
        |float c[10];
        |float d;
        |void main(){
        | a = c[b];
        |}
      """.stripMargin
    val expect = TypeMismatchInExpression(BinaryOp("=", Id("a"), ArrayCell(Id("c"), Id("b")))).getMessage
    assert(checkCkr(input, expect, 23))
  }
  test("Type Mismatch In Expression: Function call - Parameter length") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){
        |  do
        |  a = a + 1;
        |  while a <= test(3);
        |}
        |int test(){
        |  return 5;
        |}
      """.stripMargin
    val expect = TypeMismatchInExpression(CallExpr(Id("test"), List(IntLiteral(3)))).getMessage
    assert(checkCkr(input, expect, 24))
  }
  test("Type Mismatch In Expression: Function call -Wrong type bypass") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){
        |  do
        |  a = a + 1;
        |  while a <= test(6.0);
        |}
        |int test(int d){
        |  return d;
        |}
      """.stripMargin
    val expect = TypeMismatchInExpression(CallExpr(Id("test"), List(FloatLiteral(6.0f)))).getMessage
    assert(checkCkr(input, expect, 25))
  }
  test("Function not return") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){
        |  do
        |  a = a + 1;
        |  while a <= test(6);
        |}
        |int test(int d){
        |}
      """.stripMargin
    val expect = FunctionNotReturn("test").getMessage
    assert(checkCkr(input, expect, 26))
  }
  test("Function not return: missing in other path") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){
        |  do
        |  a = a + 1;
        |  while a <= test(6);
        |}
        |int test(int d){
        | if(d <10) return 8;
        |}
      """.stripMargin
    val expect = FunctionNotReturn("test").getMessage
    assert(checkCkr(input, expect, 27))
  }
  test("Break not in loop") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){
        |  do
        |  a = a + 1;
        |  while a <= test(6);
        | break;
        |}
        |int test(int d){
        | return 8;
        |}
      """.stripMargin
    val expect = BreakNotInLoop.getMessage
    assert(checkCkr(input, expect, 28))
  }
  test("Continue not in loop") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){
        |  do
        |  a = a + 1;
        |  while a <= test(6);
        | continue;
        |}
        |int test(int d){
        | return 8;
        |}
      """.stripMargin
    val expect = ContinueNotInLoop.getMessage
    assert(checkCkr(input, expect, 29))
  }
  test("Unreachable statement: After return") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){
        |  do
        |  a = a + 1;
        |  while a <= test(6);
        |}
        |int test(int d){
        | return 8;
        | d = 8;
        |}
      """.stripMargin
    val expect = UnreachableStatement(BinaryOp("=", Id("d"), IntLiteral(8))).getMessage
    assert(checkCkr(input, expect, 30))
  }
  test("Unreachable Function") {
    val input =
      """int a, b;
        |float c[10];
        |void main(){
        |  do
        |  a = a + 1;
        |  while a <= 10;
        |}
        |int test(int d){
        | if(d < 8) return d;
        | return 8;
        |}
      """.stripMargin
    val expect = UnreachableFunction("test").getMessage
    assert(checkCkr(input, expect, 31))
  }

}