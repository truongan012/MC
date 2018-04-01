package mc.astgen
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.ParserRuleContext
import java.io.{PrintWriter,File}
import org.antlr.v4.runtime.ANTLRFileStream
import mc.utils._
import scala.collection.JavaConverters._
import org.antlr.v4.runtime.tree._
import mc.parser._
import mc.parser.MCParser._

class ASTGeneration extends MCBaseVisitor[Any] {

  override def visitProgram(ctx:ProgramContext) = 
  			Program(List(FuncDecl(Id("main"),List(),VoidType,Block(List(),List()))))

	

}