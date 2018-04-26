package mc.checker

/**
  * @author nhphung
  * Student name: Lam Truong An
  * Student ID: 1570733
  */

import mc.parser._
import mc.utils._
import java.io.{File, PrintWriter}

//import mc.codegen.Val
import org.antlr.v4.runtime.ANTLRFileStream
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree._

import scala.collection.JavaConverters._


class StaticChecker(ast: AST) extends BaseVisitor with Utils {

  def check() = visit(ast, null)

  override def visitProgram(ast: Program, c: Any): Any = throw BreakNotInLoop


}
