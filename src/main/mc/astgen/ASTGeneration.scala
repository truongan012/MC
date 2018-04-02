package mc.astgen

import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.ParserRuleContext
import java.io.{PrintWriter, File}
import org.antlr.v4.runtime.ANTLRFileStream
import mc.utils._
import scala.collection.JavaConverters._
import org.antlr.v4.runtime.tree._
import mc.parser._
import mc.parser.MCParser._

class ASTGeneration extends MCBaseVisitor[Any] {

  override def visitProgram(ctx: ProgramContext) =
    Program(ctx.declaration.asScala.map(_.accept(this).asInstanceOf[Decl]).toList)

  override def visitDeclaration(ctx: DeclarationContext) =
    if (ctx.varDecl != null) ctx.varDecl.accept(this)
    else ctx.funcDecl.accept(this)

  override def visitVarDecl(ctx: VarDeclContext) = {
    ctx.varList.accept(this)
    ctx.primitiveTypes.accept(this)
  }

  override def visitVarList(ctx: VarListContext) =
    ctx.variable.asScala.map(_.accept(this).asInstanceOf[Id]).toList

  override def visitVariable(ctx: VariableContext) =
    if (ctx.getChildCount == 1) null
    else null

  override def visitStatement(ctx: StatementContext) =
    if (ctx.returnStmt != null) ctx.returnStmt.accept(this)
    else if (ctx.blockStmt != null) ctx.blockStmt.accept(this)
    else if (ctx.ifStmt != null) ctx.ifStmt.accept(this)
    else if (ctx.doWhileStmt != null) ctx.doWhileStmt.accept(this)
    else if (ctx.forStmt != null) ctx.forStmt.accept(this)
    else if (ctx.expStmt != null) ctx.expStmt.accept(this)
    else if (ctx.breakStmt != null) ctx.breakStmt.accept(this)
    else ctx.continueStmt.accept(this)

  override def visitIfStmt(ctx: IfStmtContext) =
    If(ctx.expression.accept(this).asInstanceOf[Expr],
      ctx.statement(0).accept(this).asInstanceOf[Stmt],
      if (ctx.statement(1) != null) Option(ctx.statement(1).accept(this).asInstanceOf[Stmt]) else None
    )

  override def visitDoWhileStmt(ctx: DoWhileStmtContext) =
    Dowhile(ctx.statement.asScala.map(_.accept(this).asInstanceOf[Stmt]).toList,
      ctx.expression.accept(this).asInstanceOf[Expr])

  override def visitForStmt(ctx: ForStmtContext) =
    For(ctx.expression(0).accept(this).asInstanceOf[Expr],
      ctx.expression(1).accept(this).asInstanceOf[Expr],
      ctx.expression(2).accept(this).asInstanceOf[Expr],
      ctx.statement.accept(this).asInstanceOf[Stmt]
    )

  override def visitBreakStmt(ctx: BreakStmtContext) = Break

  override def visitContinueStmt(ctx: ContinueStmtContext) = Continue

  override def visitReturnStmt(ctx: ReturnStmtContext) =
    Return(if (ctx.expression != null) Option(ctx.expression.accept(this).asInstanceOf[Expr]) else None)

  override def visitExpStmt(ctx: ExpStmtContext) = ctx.expression.accept(this).asInstanceOf[Expr]

  override def visitBlockStmt(ctx: BlockStmtContext) =
    Block(
      ctx.varDecl.asScala.map(_.accept(this).asInstanceOf[Decl]).toList,
      ctx.statement.asScala.map(_.accept(this).asInstanceOf[Stmt]).toList
    )

  override def visitExpression(ctx: ExpressionContext) = // Khong co Expression Statement
    if (ctx.getChildCount == 1) ctx.expression1.accept(this)
    else
      BinaryOp(ctx.ASSIGN_OP.getText,
        ctx.expression1.accept(this).asInstanceOf[Expr],
        ctx.expression.accept(this).asInstanceOf[Expr]
      )

  override def visitExpression1(ctx: Expression1Context) =
    if (ctx.getChildCount == 1) ctx.expression2.accept(this)
    else
      BinaryOp(ctx.OR_OP.getText,
        ctx.expression1.accept(this).asInstanceOf[Expr],
        ctx.expression2.accept(this).asInstanceOf[Expr]
      )

  override def visitExpression2(ctx: Expression2Context) =
    if (ctx.getChildCount == 1) ctx.expression3.accept(this)
    else
      BinaryOp(ctx.AND_OP.getText,
        ctx.expression2.accept(this).asInstanceOf[Expr],
        ctx.expression3.accept(this).asInstanceOf[Expr]
      )

  override def visitExpression3(ctx: Expression3Context) =
    if (ctx.getChildCount == 1) ctx.expression4(0).accept(this)
    else
      BinaryOp(if (ctx.EQ_OP != null) ctx.EQ_OP.getText else ctx.NEQ_OP.getText,
        ctx.expression4(0).accept(this).asInstanceOf[Expr],
        ctx.expression4(1).accept(this).asInstanceOf[Expr]
      )

  override def visitExpression4(ctx: Expression4Context) =
    if (ctx.getChildCount == 1) ctx.expression5(0).accept(this)
    else
      BinaryOp(
        if (ctx.LESS_OP != null) ctx.LESS_OP.getText
        else if (ctx.LESS_EQ_OP != null) ctx.LESS_EQ_OP.getText
        else if (ctx.GREATER_OP != null) ctx.GREATER_OP.getText
        else ctx.GREATER_EQ_OP.getText,
        ctx.expression5(0).accept(this).asInstanceOf[Expr],
        ctx.expression5(1).accept(this).asInstanceOf[Expr]
      )

  override def visitExpression5(ctx: Expression5Context) =
    if (ctx.getChildCount == 1) ctx.expression6.accept(this)
    else
      BinaryOp(if (ctx.ADD_OP != null) ctx.ADD_OP.getText else ctx.SUB_OP.getText,
        ctx.expression5.accept(this).asInstanceOf[Expr],
        ctx.expression6.accept(this).asInstanceOf[Expr]
      )

  override def visitExpression6(ctx: Expression6Context) =
    if (ctx.getChildCount == 1) ctx.expression7.accept(this)
    else
      BinaryOp(
        if (ctx.MUL_OP != null) ctx.MUL_OP.getText
        else if (ctx.DIV_OP != null) ctx.DIV_OP.getText
        else ctx.MOD_OP.getText,
        ctx.expression6.accept(this).asInstanceOf[Expr],
        ctx.expression7.accept(this).asInstanceOf[Expr]
      )

  override def visitExpression7(ctx: Expression7Context) =
    if (ctx.getChildCount == 1) ctx.expression8.accept(this)
    else
      UnaryOp(if (ctx.NOT_OP != null) ctx.NOT_OP.getText else ctx.SUB_OP.getText,
        ctx.expression7.accept(this).asInstanceOf[Expr]
      )

  override def visitExpression8(ctx: Expression8Context) =
    if (ctx.getChildCount == 1) ctx.expression9.accept(this)
    else
      ArrayCell(
        ctx.expression9.accept(this).asInstanceOf[Expr],
        ctx.expression5.accept(this).asInstanceOf[Expr]
      )

  override def visitExpression9(ctx: Expression9Context) =
    if (ctx.getChildCount == 1) ctx.expression10.accept(this)
    else
      CallExpr(
        Id(ctx.ID.getText),
        ctx.expList.accept(this).asInstanceOf[List[Expr]]
      )

  override def visitExpression10(ctx: Expression10Context) =
    if (ctx.getChildCount == 1) ctx.atomic.accept(this)
    else ctx.expression.accept(this)

  override def visitAtomic(ctx: AtomicContext) =
    if (ctx.ID != null) Id(ctx.ID.getText)
    else if(ctx.INT_LIT != null) IntLiteral(ctx.INT_LIT.getText.toInt)
    else if (ctx.FLT_LIT != null) FloatLiteral(ctx.FLT_LIT.getText.toFloat)
    else if(ctx.BOOL_LIT != null) BooleanLiteral(ctx.BOOL_LIT.getText.toBoolean)
    else StringLiteral(ctx.STR_LIT.getText)

  override def visitExpList(ctx: ExpListContext) =
    ctx.expression.asScala.map(_.accept(this).asInstanceOf[Expr]).toList
}