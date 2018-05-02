package mc.checker

/**
  * @author nhphung
  * Student name: Lam Truong An
  * Student ID: 1570733
  */

import mc.parser._
import mc.utils._
import java.io.{File, PrintWriter}

import com.sun.org.apache.xalan.internal.xsltc.dom.AdaptiveResultTreeImpl

//import mc.codegen.Val
import org.antlr.v4.runtime.ANTLRFileStream
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree._

import scala.collection.JavaConverters._

case class Symbol(name: String, typ: Type)

case class FunctionType(input: List[Type], output: Type) extends Type

class StaticChecker(ast: AST) extends MyBaseVistor with MyUtils {
  val buidInFunc = List(
    FuncDecl(Id("getInt"), List(), IntType, Block(List(), List())),
    FuncDecl(Id("putInt"), List(VarDecl(Id("i"), IntType)), VoidType, Block(List(), List())),
    FuncDecl(Id("putIntLn"), List(VarDecl(Id("i"), IntType)), VoidType, Block(List(), List())),
    FuncDecl(Id("getFloat"), List(), IntType, Block(List(), List())),
    FuncDecl(Id("putFloat"), List(VarDecl(Id("f"), FloatType)), VoidType, Block(List(), List())),
    FuncDecl(Id("putFloatLn"), List(VarDecl(Id("f"), FloatType)), VoidType, Block(List(), List())),
    FuncDecl(Id("putBool"), List(VarDecl(Id("b"), BoolType)), VoidType, Block(List(), List())),
    FuncDecl(Id("putBoolLn"), List(VarDecl(Id("b"), BoolType)), VoidType, Block(List(), List())),
    FuncDecl(Id("putString"), List(VarDecl(Id("s"), StringType)), VoidType, Block(List(), List())),
    FuncDecl(Id("putStringLn"), List(VarDecl(Id("s"), StringType)), VoidType, Block(List(), List())),
    FuncDecl(Id("putLn"), List(), VoidType, Block(List(), List()))
  )

  def check() = {
    //Check Redeclare global variable, function, no entry point
    val globalChecker = new GlobalChecker().visit(ast, buildInEnv)

    //Check Redclare local variable, parameter, type mismatch in statement, expression
    val typeChecker = new TypeChecker().visit(ast, globalChecker)

    //Check function not return; break, continue not in loop, unreachable statement
    val otherchecker = new OtherChecker().visit(ast, null)

    //Check unreachable function
    val envWoBuildInFunc = getEnvWoBuildInFunc(globalChecker.asInstanceOf[List[List[Symbol]]].flatten)
    val unReachableChecker = new UnreachableFuncChecker().visit(ast, envWoBuildInFunc)
  }

  def buildInEnv() = buidInFunc.map(f => convertToSymbol(f))

  def getEnvWoBuildInFunc[T](env: List[T]) = env.slice(0, env.size - buidInFunc.size)

}

class GlobalChecker extends MyBaseVistor with MyUtils {

  override def visitProgram(ast: Program, c: Any): Any = {
    val buildInEnv = c.asInstanceOf[List[Symbol]]
    val globalEnv = ast.decl.foldLeft(buildInEnv)((el, d) => d.accept(this, el).asInstanceOf[List[Symbol]])
    //println(globalEnv)
    checkNoEntryPoint(globalEnv)
    List(globalEnv)
  }

  override def visitFuncDecl(ast: FuncDecl, c: Any): Any = {
    val env = c.asInstanceOf[List[Symbol]]
    lookupToInsert(convertToSymbol(ast), env, Function)
  }

  override def visitVarDecl(ast: VarDecl, c: Any): Any = {
    val env = c.asInstanceOf[List[Symbol]]
    lookupToInsert(convertToSymbol(ast), env, Variable)
  }
}

class TypeChecker extends MyBaseVistor with MyUtils {
  override def visitProgram(ast: Program, c: Any): Any = {
    val env = c.asInstanceOf[List[List[Symbol]]]
    val func = ast.decl.filter(_.isInstanceOf[FuncDecl])
    func.foldLeft(env)((el, fd) =>
      fd.accept(this, el).asInstanceOf[List[List[Symbol]]])
  }

  override def visitFuncDecl(ast: FuncDecl, c: Any): Any = {
    val env = c.asInstanceOf[List[List[Symbol]]]

    //Environment for function is from here, including global environment
    val paraEnv =
      if (!ast.param.isEmpty)
        ast.param.foldLeft(List[Symbol]())((el, d) =>
          lookupToInsert(convertToSymbol(d), el, Parameter)) :: env
      else List[Symbol]() :: env

    //Redeclare check for which decl in function block
    val funcEnv =
      if (!ast.body.asInstanceOf[Block].decl.isEmpty)
        ast.body.asInstanceOf[Block].decl.foldLeft(paraEnv)((el, d) =>
          d.accept(this, el).asInstanceOf[List[List[Symbol]]])
      else paraEnv

    //Check for each statement in function block
    if (!ast.body.asInstanceOf[Block].stmt.isEmpty)
      ast.body.asInstanceOf[Block].stmt.map(_.accept(this, (funcEnv, ast.returnType)))
    c
  }

  override def visitVarDecl(ast: VarDecl, c: Any): Any = {
    val env = c.asInstanceOf[List[List[Symbol]]]
    val curEnv = env.head
    lookupToInsert(convertToSymbol(ast), curEnv, Variable) :: env
  }

  override def visitBlock(ast: Block, c: Any): Any = {
    val env = c.asInstanceOf[(List[List[Symbol]],Type)]._1
    val newEnv =
      if (!ast.decl.isEmpty)
        ast.decl.foldLeft(env)((el, d) =>
          d.accept(this, env).asInstanceOf[List[List[Symbol]]]) :: env
      else List[Symbol]() :: env
    if (!ast.decl.isEmpty)
      ast.stmt.map(_.accept(this, newEnv))

    c
  }

  override def visitIf(ast: If, c: Any): Any = {
    if (ast.expr.accept(this, c).asInstanceOf[Type] != BoolType) throw TypeMismatchInStatement(ast)
    ast.thenStmt.accept(this, c)
    if (ast.elseStmt != None) ast.elseStmt.get.accept(this, c)
  }

  override def visitFor(ast: For, c: Any): Any = {
    if (ast.expr1.accept(this, c).asInstanceOf[Type] != IntType) throw TypeMismatchInStatement(ast)
    if (ast.expr2.accept(this, c).asInstanceOf[Type] != BoolType) throw TypeMismatchInStatement(ast)
    if (ast.expr3.accept(this, c).asInstanceOf[Type] != IntType) throw TypeMismatchInStatement(ast)
    ast.loop.accept(this, c)
  }

  override def visitDowhile(ast: Dowhile, c: Any): Any = {
    if (ast.exp.accept(this, c).asInstanceOf[Type] != BoolType) throw TypeMismatchInStatement(ast)
    ast.sl.map(_.accept(this, c))
  }

  override def visitReturn(ast: Return, c: Any): Any = {
    val (tempEnv, retType) = c.asInstanceOf[(List[List[Symbol]], Type)]
    val env = tempEnv.flatten
    val expType = if (ast.expr != None) ast.expr.get.accept(this, c).asInstanceOf[Type] else None
    //println("Ret: "+ retType + "--" + "Exp: " +expType)
    (retType, expType) match {
      case (VoidType, None) => //Do Nothing
      case (IntType, IntType) => //Do Nothing
      case (FloatType, IntType) => //Do Nothing
      case (FloatType, FloatType) => //Do Nothing
      case (BoolType, BoolType) => //Do Nothing
      case (StringType, StringType) => //Do Nothing
      case (ArrayPointerType(IntType), ArrayType(_, IntType)) => //Do Nothing
      case (ArrayPointerType(IntType), ArrayPointerType(IntType)) => //Do Nothing
      case (ArrayPointerType(FloatType), ArrayType(_, IntType)) => //Do Nothing
      case (ArrayPointerType(FloatType), ArrayPointerType(IntType)) => //Do Nothing
      case (ArrayPointerType(FloatType), ArrayType(_, FloatType)) => //Do Nothing
      case (ArrayPointerType(FloatType), ArrayPointerType(FloatType)) => //Do Nothing
      case (ArrayPointerType(BoolType), ArrayType(_, BoolType)) => //Do Nothing
      case (ArrayPointerType(BoolType), ArrayPointerType(BoolType)) => //Do Nothing
      case (ArrayPointerType(StringType), ArrayType(_, StringType)) => //Do Nothing
      case (ArrayPointerType(StringType), ArrayPointerType(StringType)) => //Do Nothing
      //other than
      case _ => throw TypeMismatchInStatement(ast)
    }
  }

  override def visitBinaryOp(ast: BinaryOp, c: Any): Any = {
    val leftType = ast.left.accept(this, c).asInstanceOf[Type]
    val rightType = ast.right.accept(this, c).asInstanceOf[Type]
    (ast.op, leftType, rightType) match {
      //Numeric
      case ("+" | "-" | "*" | "/" | "%", IntType, IntType) => IntType
      case ("+" | "-" | "*" | "/", FloatType, FloatType) => FloatType
      case ("+" | "-" | "*" | "/", FloatType, IntType) => FloatType
      case ("+" | "-" | "*" | "/", IntType, FloatType) => FloatType
      //Relation
      case ("==" | "!=" | ">" | "<" | ">=" | "<=", IntType, IntType) => BoolType
      case ("==" | "!=", BoolType, BoolType) => BoolType
      case (">" | "<" | ">=" | "<=", FloatType, FloatType) => BoolType
      case (">" | "<" | ">=" | "<=", IntType, FloatType) => BoolType
      case (">" | "<" | ">=" | "<=", FloatType, IntType) => BoolType
      //Logic
      case ("&&" | "||" | "!", BoolType, BoolType) => BoolType
      //Assign
      case ("=", IntType, IntType) => IntType
      case ("=", FloatType, IntType) => FloatType
      case ("=", FloatType, FloatType) => FloatType
      case ("=", BoolType, BoolType) => BoolType
      case ("=", StringType, StringType) => StringType
      //other than
      case _ => throw TypeMismatchInExpression(ast)
    }
  }

  override def visitUnaryOp(ast: UnaryOp, c: Any): Any = {
    val expType = ast.body.accept(this, c).asInstanceOf[Type]
    (ast.op, expType) match {
      case ("-", IntType) => IntType
      case ("-", FloatType) => FloatType
      case ("!", BoolType) => BoolType
      case _ => throw TypeMismatchInExpression(ast)
    }
  }

  override def visitCallExpr(ast: CallExpr, c: Any): Any = {
    val env = c.asInstanceOf[(List[List[Symbol]],Type)]._1.flatten
    val func = lookupToPick(ast.method.name, env, Function)
    val paraTypes = func.typ.asInstanceOf[FunctionType].input
    val argTypes = ast.params.map(_.accept(this, c)).asInstanceOf[List[Type]]
    if (argTypes.length != paraTypes.length) throw TypeMismatchInExpression(ast)
    paraTypes.zip(argTypes).map(_ match {
      case (IntType, IntType) => //Do Nothing
      case (FloatType, IntType) => //Do Nothing
      case (FloatType, FloatType) => //Do Nothing
      case (BoolType, BoolType) => //Do Nothing
      case (StringType, StringType) => //Do Nothing
      case (ArrayPointerType(IntType), ArrayType(_, IntType)) => //Do Nothing
      case (ArrayPointerType(IntType), ArrayPointerType(IntType)) => //Do Nothing
      case (ArrayPointerType(FloatType), ArrayType(_, IntType)) => //Do Nothing
      case (ArrayPointerType(FloatType), ArrayPointerType(IntType)) => //Do Nothing
      case (ArrayPointerType(FloatType), ArrayType(_, FloatType)) => //Do Nothing
      case (ArrayPointerType(FloatType), ArrayPointerType(FloatType)) => //Do Nothing
      case (ArrayPointerType(BoolType), ArrayType(_, BoolType)) => //Do Nothing
      case (ArrayPointerType(BoolType), ArrayPointerType(BoolType)) => //Do Nothing
      case (ArrayPointerType(StringType), ArrayType(_, StringType)) => //Do Nothing
      case (ArrayPointerType(StringType), ArrayPointerType(StringType)) => //Do Nothing
      //other than
      case _ => throw TypeMismatchInExpression(ast)
    }
    )
    func.typ.asInstanceOf[FunctionType].output
  }

  override def visitId(ast: Id, c: Any): Any = {
    val env = c.asInstanceOf[(List[List[Symbol]],Type)]._1.flatten
    val id = lookupToPick(ast.name, env, Identifier)
    id.typ
  }

  override def visitArrayCell(ast: ArrayCell, c: Any): Any = {
    val arrType = ast.arr.accept(this, c).asInstanceOf[Type]
    if (ast.idx.accept(this, c).asInstanceOf[Type] != IntType)
      throw TypeMismatchInExpression(ast)
    arrType match {
      case ArrayPointerType(typ) => typ
      case ArrayType(_, typ) => typ
      case _ => throw TypeMismatchInExpression(ast)
    }
  }
}

class OtherChecker extends MyBaseVistor with MyUtils {
  override def visitProgram(ast: Program, c: Any): Any = {
    ast.decl.filter(_.isInstanceOf[FuncDecl]).foldLeft(c)((el, fd) => fd.accept(this, el))
  }

  override def visitFuncDecl(ast: FuncDecl, c: Any): Any = {
    val isInLoop = false
    val hasReturn = ast.body.asInstanceOf[Block].accept(this, isInLoop).asInstanceOf[Boolean]
    if (ast.returnType != VoidType)
      if (!hasReturn) throw FunctionNotReturn(ast.name.name)

  }

  override def visitBlock(ast: Block, c: Any): Any = {
    ast.stmt.foldLeft(false)((a, b) =>
      b.accept(this, c).asInstanceOf[Boolean] || (if (a) throw UnreachableStatement(b) else a)
    )
  }

  override def visitIf(ast: If, c: Any): Any = {
    val thenCase = ast.thenStmt.accept(this, c).asInstanceOf[Boolean]
    val elseCase = if (ast.elseStmt != None) ast.elseStmt.get.accept(this, c).asInstanceOf[Boolean] else false
//    if (ast.expr.isInstanceOf[BooleanLiteral])
//      if (ast.expr.asInstanceOf[BooleanLiteral].value == false && ast.elseStmt == None) throw UnreachableStatement(ast)
    thenCase && elseCase
  }

  override def visitFor(ast: For, c: Any): Any = {
    ast.loop.accept(this, true).asInstanceOf[Boolean]
    false
  }

  override def visitBreak(ast: Break.type, c: Any): Any = {
    checkInLoop(ast, c.asInstanceOf[Boolean])
    false
  }

  override def visitContinue(ast: Continue.type, c: Any): Any = {
    checkInLoop(ast, c.asInstanceOf[Boolean])
    false
  }

  override def visitReturn(ast: Return, c: Any): Any = {
    true
  }

  override def visitDowhile(ast: Dowhile, c: Any): Any = {
    ast.sl.foldLeft(false)((a, b) => b.accept(this, true).asInstanceOf[Boolean] || (if (a) throw UnreachableStatement(b) else a))
    false
  }

}

class UnreachableFuncChecker extends BaseVisitor with MyUtils {

  override def visitProgram(ast: Program, c: Any): Any = {
    val env = c.asInstanceOf[List[Symbol]].filter(_.typ.isInstanceOf[FunctionType])
      .map(_.name).asInstanceOf[List[String]]

    val funcInUse = ast.decl.filter(_.isInstanceOf[FuncDecl]).foldLeft(List[String]())((el, d) =>
      d.accept(this, el).asInstanceOf[List[String]])

    env.map(x =>
      if (x != "main" && lookup(x, funcInUse, (a: String) => a) == None)
        throw UnreachableFunction(x))
  }

  override def visitFuncDecl(ast: FuncDecl, c: Any): Any = {
    val env = c.asInstanceOf[List[String]]
    val list = ast.body.asInstanceOf[Block].accept(this, env).asInstanceOf[List[String]]
    list
  }

  override def visitBlock(ast: Block, c: Any): Any = {
    val env = c.asInstanceOf[List[String]]
    if (!ast.stmt.isEmpty)
      ast.stmt.foldLeft(env)((a, b) => b.accept(this, a).asInstanceOf[List[String]])
  }

  override def visitIf(ast: If, c: Any): Any = {
    val env = c.asInstanceOf[List[String]]
    val newEnv = ast.expr.accept(this, env).asInstanceOf[List[String]]
    val newEnv1 = ast.thenStmt.accept(this, newEnv).asInstanceOf[List[String]]
    if (ast.elseStmt != None) ast.elseStmt.get.accept(this, newEnv1).asInstanceOf[List[String]]
    else newEnv1
  }

  override def visitFor(ast: For, c: Any): Any = {
    val env = c.asInstanceOf[List[String]]
    val newEnv = ast.expr1.accept(this, env).asInstanceOf[List[String]]
    val newEnv1 = ast.expr2.accept(this, newEnv).asInstanceOf[List[String]]
    val newEnv2 = ast.expr3.accept(this, newEnv1).asInstanceOf[List[String]]
    ast.loop.accept(this, newEnv2).asInstanceOf[List[String]]
  }

  override def visitBreak(ast: Break.type, c: Any): Any = c

  override def visitContinue(ast: Continue.type, c: Any): Any = c

  override def visitReturn(ast: Return, c: Any): Any = {
    if (ast.expr != None) ast.expr.get.accept(this, c).asInstanceOf[List[String]]
    else c.asInstanceOf[List[String]]
  }

  override def visitDowhile(ast: Dowhile, c: Any): Any = {
    val env = c.asInstanceOf[List[String]]
    val newEnv = ast.sl.foldLeft(env)((a, b) => b.accept(this, a).asInstanceOf[List[String]])
    ast.exp.accept(this, newEnv).asInstanceOf[List[String]]
  }

  override def visitBinaryOp(ast: BinaryOp, c: Any): Any = {
    val env = c.asInstanceOf[List[String]]
    val newEnv = ast.left.accept(this, env).asInstanceOf[List[String]]
    ast.right.accept(this, newEnv).asInstanceOf[List[String]]
  }

  override def visitUnaryOp(ast: UnaryOp, c: Any): Any = {
    val env = c.asInstanceOf[List[String]]
    ast.body.accept(this, env).asInstanceOf[List[String]]
  }

  override def visitCallExpr(ast: CallExpr, c: Any): Any = {
    val env = c.asInstanceOf[List[String]]
    val newEnv = ast.params.foldLeft(env)((a, b) => b.accept(this, a).asInstanceOf[List[String]])
    if (lookup(ast.method.name, newEnv, (n: String) => n) == None)
      ast.method.name :: newEnv
    else newEnv
  }

  override def visitId(ast: Id, c: Any): Any = c

  override def visitArrayCell(ast: ArrayCell, c: Any): Any = {
    val env = c.asInstanceOf[List[String]]
    val newEnv = ast.idx.accept(this, env).asInstanceOf[List[String]]
    ast.arr.accept(this, newEnv).asInstanceOf[String]
  }

  override def visitIntType(ast: IntType.type, c: Any): Any = c

  override def visitFloatType(ast: FloatType.type, c: Any): Any = c

  override def visitBoolType(ast: BoolType.type, c: Any): Any = c

  override def visitStringType(ast: StringType.type, c: Any): Any = c

  override def visitIntLiteral(ast: IntLiteral, c: Any): Any = c

  override def visitFloatLiteral(ast: FloatLiteral, c: Any): Any = c

  override def visitStringLiteral(ast: StringLiteral, c: Any): Any = c

  override def visitBooleanLiteral(ast: BooleanLiteral, c: Any): Any = c
}

trait MyUtils extends Utils {
  def checkNoEntryPoint(ls: List[Symbol]) = {
    if (!ls.exists(s => s.equals(Symbol("main", FunctionType(List(), VoidType)))))
      throw NoEntryPoint
  }

  def checkInLoop(stmt: Stmt, isInLoop: Boolean) = {
    if (!isInLoop) stmt match {
      case Break => throw BreakNotInLoop
      case Continue => throw ContinueNotInLoop
    }
  }

  def lookupToInsert(symbol: Symbol, list: List[Symbol], kind: Kind) = {
    if (lookup(symbol.name, list, (s: Symbol) => s.name) != None)
      throw Redeclared(kind, symbol.name)
    else symbol :: list
  }

  def lookupToPick(name: String, list: List[Symbol], kind: Kind) = {
    val symbol = lookup(name, list, (s: Symbol) => s.name)
    if (symbol == None) throw Undeclared(kind, name)
    else symbol.get
  }
}

class MyBaseVistor extends BaseVisitor with MyUtils {
  override def visitIntLiteral(ast: IntLiteral, c: Any): Any = IntType

  override def visitFloatLiteral(ast: FloatLiteral, c: Any): Any = FloatType

  override def visitStringLiteral(ast: StringLiteral, c: Any): Any = StringType

  override def visitBooleanLiteral(ast: BooleanLiteral, c: Any): Any = BoolType

  override def visitIntType(ast: IntType.type, c: Any): Any = IntType

  override def visitFloatType(ast: FloatType.type, c: Any): Any = FloatType

  override def visitBoolType(ast: BoolType.type, c: Any): Any = BoolType

  override def visitStringType(ast: StringType.type, c: Any): Any = StringType

  override def visitVoidType(ast: VoidType.type, c: Any): Any = VoidType

  override def visitArrayType(ast: ArrayType, c: Any): Any =
    ArrayType(null, ast.eleType.accept(this, c).asInstanceOf[Type])

  override def visitArrayPointerType(ast: ArrayPointerType, c: Any): Any =
    ArrayPointerType(ast.eleType.accept(this, c).asInstanceOf[Type])

  def convertToSymbol(decl: Decl): Symbol = {
    decl match {
      case VarDecl(id, typ) =>
        Symbol(id.name, typ)
      case FuncDecl(id, paras, ret, _) =>
        Symbol(
          id.name,
          FunctionType(
            paras.map(_.varType.accept(this, null).asInstanceOf[Type]),
            ret.accept(this, null).asInstanceOf[Type]
          )
        )
    }
  }
}
