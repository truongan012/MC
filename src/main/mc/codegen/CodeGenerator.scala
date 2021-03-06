/**
  * @author Nguyen Hua Phung
  * @version 1.0
  *          23/10/2015
  *          This file provides a simple version of code generator
  *          Student name: Lam Truong An
  *          Student ID: 1570733
  */

package mc.codegen

import mc.checker._
import mc.utils._
import java.io.{PrintWriter, File}

object CodeGenerator extends Utils {
  val libName = "io"

  def gen(ast: AST, dir: File) = {

    val gl = init()
    val gc = new CodeGenVisitor(ast, gl, dir)
    gc.visit(ast, null);
  }

  def init() = List(Symbol("getInt", FuncType(List(), IntType), CName(libName)),
    Symbol("putInt", FuncType(List(IntType), VoidType), CName(libName)),
    Symbol("putIntLn", FuncType(List(IntType), VoidType), CName(libName))
  )
}

case class Symbol(name: String, typ: Type, value: Val)

case class FuncType(partype: List[Type], rettype: Type) extends Type


case class ClassType(cname: String) extends Type

case class SubContext(emit: Emitter, decl: List[Decl])

case class SubBody(frame: Frame, sym: List[Symbol])

class Access(val frame: Frame, val sym: List[Symbol], val isLeft: Boolean, val isFirst: Boolean)

trait Val

case class Index(value: Int) extends Val

case class CName(value: String) extends Val


class CodeGenVisitor(astTree: AST, env: List[Symbol], dir: File) extends BaseVisitor with Utils {

  val className = "MCClass"
  val path = dir.getPath()
  val emit = new Emitter(path + "/" + className + ".j")

  override def visitProgram(ast: Program, c: Any) = {


    emit.printout(emit.emitPROLOG(className, "java.lang.Object"))
    ast.decl.foldLeft(SubBody(null, env))((e, x) => visit(x, e).asInstanceOf[SubBody])
    // generate default constructor
    genMETHOD(
      FuncDecl(Id("<init>"), List(), null, Block(List(), List())), c, new Frame("<init>", VoidType))
    emit.emitEPILOG()
    c
  }


  /** generate code for default constructor
    *
    * @param consdecl the function declaration whose code will be generated by this method
    * @param frame    the frame where the initialization happen
    * @param o        the referencing environment
    */
  def genMETHOD(consdecl: FuncDecl, o: Any, frame: Frame) = {

    val isInit = consdecl.returnType == null
    val isMain = consdecl.name.name == "main" && consdecl.param.length == 0 && consdecl.returnType == VoidType
    val returnType = if (isInit) VoidType else consdecl.returnType
    val methodName = if (isInit) "<init>" else consdecl.name.name
    val intype = if (isMain) List() else consdecl.param.map(_.varType)
    val mtype = FuncType(intype, if (isMain) VoidType else returnType)

    emit.printout(emit.emitMETHOD(methodName, mtype, !isInit, frame))

    frame.enterScope(true);

    val glenv = o.asInstanceOf[List[Symbol]]

    // Generate code for parameter declarations
    if (isInit) emit.printout(emit.emitVAR(frame.getNewIndex, "this", ClassType(className), frame.getStartLabel, frame.getEndLabel, frame))

    val body = consdecl.body.asInstanceOf[Block]

    emit.printout(emit.emitLABEL(frame.getStartLabel(), frame))
    //Generate code for statements
    if (isInit) {
      emit.printout(emit.emitREADVAR("this", ClassType(className), 0, frame))
      emit.printout(emit.emitINVOKESPECIAL(frame))
    }
    body.stmt.map(x => visit(x, SubBody(frame, glenv)))

    emit.printout(emit.emitLABEL(frame.getEndLabel(), frame))
    if (returnType == VoidType) emit.printout(emit.emitRETURN(VoidType, frame));
    emit.printout(emit.emitENDMETHOD(frame));
    frame.exitScope();


  }


  override def visitFuncDecl(ast: FuncDecl, o: Any) = {
    val subctxt = o.asInstanceOf[SubBody]
    val frame = new Frame(ast.name.name, ast.returnType)
    genMETHOD(ast, subctxt.sym, frame)
    SubBody(null, Symbol(ast.name.name, FuncType(List(), ast.returnType), CName(className)) :: subctxt.sym)
  }


  override def visitCallExpr(ast: CallExpr, o: Any) = {
    val ctxt = o.asInstanceOf[SubBody]
    val frame = ctxt.frame
    val nenv = ctxt.sym
    val sym = lookup(ast.method.name, nenv, (x: Symbol) => x.name).get
    val cname = sym.value.asInstanceOf[CName].value
    val ctype = sym.typ

    val in = ast.params.foldLeft(("", List[Type]()))((y, x) => {
      val (str1, typ1) = visit(x, new Access(frame, nenv, false, true)).asInstanceOf[(String, Type)]
      (y._1 + str1, y._2 :+ typ1)
    }
    )
    emit.printout(in._1)

    emit.printout(emit.emitINVOKESTATIC(cname + "/" + ast.method, ctype, frame))


  }

  override def visitIntLiteral(ast: IntLiteral, o: Any) = {
    val ctxt = o.asInstanceOf[Access]
    val frame = ctxt.frame
    (emit.emitPUSHICONST(ast.value, frame), IntType)
  }

}