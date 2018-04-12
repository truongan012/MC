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
    Program(
      ctx.declaration.asScala.toList.flatMap(_.accept(this).asInstanceOf[List[Decl]])
    )

  override def visitDeclaration(ctx: DeclarationContext): Any =
    ctx.getChild(0).accept(this)

  override def visitVarDecl(ctx: VarDeclContext): List[VarDecl] =
    ctx.variable.asScala.toList
      .map(_.accept(this).asInstanceOf[(Id, IntLiteral)])
      .map(x => VarDecl(x._1,
        if (x._2.isInstanceOf[IntLiteral])
          ArrayType(x._2, ctx.primitiveTypes.accept(this).asInstanceOf[Type])
        else ctx.primitiveTypes.accept(this).asInstanceOf[Type])
      )

  override def visitVariable(ctx: VariableContext): (Id, Product with Serializable) =
    (Id(ctx.ID.getText), if (ctx.INT_LIT != null) IntLiteral(ctx.INT_LIT.getText.toInt) else None)

  override def visitFuncDecl(ctx: FuncDeclContext) =
    List(
      FuncDecl(
        Id(ctx.ID.getText),
        if (ctx.paraList != null) ctx.paraList.accept(this).asInstanceOf[List[VarDecl]] else List(),
        ctx.types.accept(this).asInstanceOf[Type],
        ctx.blockStmt.accept(this).asInstanceOf[Stmt],
      )
    )

  override def visitParaList(ctx: ParaListContext): List[VarDecl] =
    ctx.paraDecl.asScala.toList.map(_.accept(this).asInstanceOf[VarDecl])

  override def visitParaDecl(ctx: ParaDeclContext) =
    VarDecl(
      Id(ctx.ID.getText),
      if (ctx.LSB != null) ArrayPointerType(ctx.primitiveTypes.accept(this).asInstanceOf[Type])
      else ctx.primitiveTypes.accept(this).asInstanceOf[Type]
    )

  override def visitStatement(ctx: StatementContext): Any =
    ctx.getChild(0).accept(this)

  override def visitIfStmt(ctx: IfStmtContext) =
    If(ctx.expression.accept(this).asInstanceOf[Expr],
      ctx.statement(0).accept(this).asInstanceOf[Stmt],
      if (ctx.statement(1) != null) Option(ctx.statement(1).accept(this).asInstanceOf[Stmt]) else None
    )

  override def visitDoWhileStmt(ctx: DoWhileStmtContext) =
    Dowhile(ctx.statement.asScala.toList.map(_.accept(this).asInstanceOf[Stmt]),
      ctx.expression.accept(this).asInstanceOf[Expr])

  override def visitForStmt(ctx: ForStmtContext) =
    For(
      ctx.expression(0).accept(this).asInstanceOf[Expr],
      ctx.expression(1).accept(this).asInstanceOf[Expr],
      ctx.expression(2).accept(this).asInstanceOf[Expr],
      ctx.statement.accept(this).asInstanceOf[Stmt]
    )

  override def visitBreakStmt(ctx: BreakStmtContext): Break.type = Break

  override def visitContinueStmt(ctx: ContinueStmtContext): Continue.type = Continue

  override def visitReturnStmt(ctx: ReturnStmtContext) =
    Return(if (ctx.expression != null) Option(ctx.expression.accept(this).asInstanceOf[Expr]) else None)

  override def visitExpStmt(ctx: ExpStmtContext): Any = ctx.expression.accept(this)

  override def visitBlockStmt(ctx: BlockStmtContext) =
    Block(
      if (ctx.varDecl != null) ctx.varDecl.asScala.toList.flatMap(_.accept(this).asInstanceOf[List[Decl]]) else List(),
      if (ctx.statement != null) ctx.statement.asScala.toList.map(_.accept(this).asInstanceOf[Stmt]) else List()
    )

  override def visitTypes(ctx: TypesContext): Any =
    if (ctx.VOID != null) VoidType
    else if (ctx.LSB != null) ArrayPointerType(ctx.primitiveTypes.accept(this).asInstanceOf[Type])
    else ctx.primitiveTypes.accept(this)

  override def visitPrimitiveTypes(ctx: PrimitiveTypesContext): Type =
    if (ctx.INT != null) IntType
    else if (ctx.FLOAT != null) FloatType
    else if (ctx.BOOLEAN != null) BoolType
    else StringType

  override def visitExpression(ctx: ExpressionContext): Any = // Khong co Expression Statement
    if (ctx.getChildCount == 1) ctx.expression1.accept(this)
    else
      BinaryOp(
        ctx.ASSIGN_OP.getText,
        ctx.expression1.accept(this).asInstanceOf[Expr],
        ctx.expression.accept(this).asInstanceOf[Expr]
      )

  override def visitExpression1(ctx: Expression1Context): Any =
    if (ctx.getChildCount == 1) ctx.expression2.accept(this)
    else
      BinaryOp(
        ctx.OR_OP.getText,
        ctx.expression1.accept(this).asInstanceOf[Expr],
        ctx.expression2.accept(this).asInstanceOf[Expr]
      )

  override def visitExpression2(ctx: Expression2Context): Any =
    if (ctx.getChildCount == 1) ctx.expression3.accept(this)
    else
      BinaryOp(
        ctx.AND_OP.getText,
        ctx.expression2.accept(this).asInstanceOf[Expr],
        ctx.expression3.accept(this).asInstanceOf[Expr]
      )

  override def visitExpression3(ctx: Expression3Context): Any =
    if (ctx.getChildCount == 1) ctx.expression4(0).accept(this)
    else
      BinaryOp(
        if (ctx.EQ_OP != null) ctx.EQ_OP.getText else ctx.NEQ_OP.getText,
        ctx.expression4(0).accept(this).asInstanceOf[Expr],
        ctx.expression4(1).accept(this).asInstanceOf[Expr]
      )

  override def visitExpression4(ctx: Expression4Context): Any =
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

  override def visitExpression5(ctx: Expression5Context): Any =
    if (ctx.getChildCount == 1) ctx.expression6.accept(this)
    else
      BinaryOp(
        if (ctx.ADD_OP != null) ctx.ADD_OP.getText else ctx.SUB_OP.getText,
        ctx.expression5.accept(this).asInstanceOf[Expr],
        ctx.expression6.accept(this).asInstanceOf[Expr]
      )

  override def visitExpression6(ctx: Expression6Context): Any =
    if (ctx.getChildCount == 1) ctx.expression7.accept(this)
    else
      BinaryOp(
        if (ctx.MUL_OP != null) ctx.MUL_OP.getText
        else if (ctx.DIV_OP != null) ctx.DIV_OP.getText
        else ctx.MOD_OP.getText,
        ctx.expression6.accept(this).asInstanceOf[Expr],
        ctx.expression7.accept(this).asInstanceOf[Expr]
      )

  override def visitExpression7(ctx: Expression7Context): Any =
    if (ctx.getChildCount == 1) ctx.expression8.accept(this)
    else
      UnaryOp(
        if (ctx.NOT_OP != null) ctx.NOT_OP.getText else ctx.SUB_OP.getText,
        ctx.expression7.accept(this).asInstanceOf[Expr]
      )

  override def visitExpression8(ctx: Expression8Context): Any =
    if (ctx.getChildCount == 1) ctx.expression9.accept(this)
    else
      ArrayCell(
        ctx.expression9.accept(this).asInstanceOf[Expr],
        ctx.expression5.accept(this).asInstanceOf[Expr]
      )

  override def visitExpression9(ctx: Expression9Context): Any =
    if (ctx.getChildCount == 1) ctx.expression10.accept(this)
    else
      CallExpr(
        Id(ctx.ID.getText),
        if (ctx.expList != null) ctx.expList.accept(this).asInstanceOf[List[Expr]] else List()
      )

  override def visitExpression10(ctx: Expression10Context): Any =
    if (ctx.getChildCount == 1) ctx.atomic.accept(this)
    else ctx.expression.accept(this)

  override def visitAtomic(ctx: AtomicContext): Expr =
    if (ctx.ID != null) Id(ctx.ID.getText)
    else if (ctx.INT_LIT != null) IntLiteral(ctx.INT_LIT.getText.toInt)
    else if (ctx.FLT_LIT != null) FloatLiteral(ctx.FLT_LIT.getText.toFloat)
    else if (ctx.BOOL_LIT != null) BooleanLiteral(ctx.BOOL_LIT.getText.toBoolean)
    else StringLiteral(ctx.STR_LIT.getText)

  override def visitExpList(ctx: ExpListContext): List[Expr] =
    ctx.expression.asScala.toList.map(_.accept(this).asInstanceOf[Expr])
}