package bpReduce
package writer

import ast._
import bpReduce.ast.Stmt._
import bpReduce.ast.Expr._
import bpReduce.ast.Expr.Or
import bpReduce.ast.Expr.Impl
import bpReduce.ast.Stmt.Call
import bpReduce.ast.Expr.And
import bpReduce.ast.Stmt.Assume
import bpReduce.ast.Expr.Xor
import bpReduce.ast.Stmt.Dead
import bpReduce.ast.Stmt.StartThread
import bpReduce.ast.Stmt.Goto
import bpReduce.ast.VariableHolder
import bpReduce.ast.Stmt.Assign
import bpReduce.ast.Stmt.Assert
import bpReduce.ast.Stmt.If
import bpReduce.ast.Stmt.Return
import bpReduce.ast.Program
import org.apache.commons.io.FileUtils
import java.io.File
import scala.collection.JavaConverters._

object Formatter {

  private val end = ";"

  def apply(program: Program): IndexedSeq[String] = {
    val globals = format(program.globals).map(_ + end)
    val functions = for {
      f <- program.functions
    } yield {
      val locals = format(f.locals).map("\t" + _)
      val returns = if (f.returns == 0) "void" else s"bool<${f.returns}>"
      val args = f.args.mkString(", ")
      val header = s"$returns ${f.name}($args) begin"
      val content = (locals ++ f.stmts.map(formatWithLabels)).map(_ + end)
      header +: content :+ "end"
    }
    val programAsLines = globals ++ functions.flatten
    programAsLines.toIndexedSeq
  }

  def format(program: Program): String = {
    val programAsLines = apply(program)
    programAsLines.mkString("\n")
  }

  def format(vars: VariableHolder): Seq[String] = {
    for {
      v <- vars.vars
    } yield {
      s"decl ${v.name}"
    }
  }

  def formatWithLabels(stmt: LabelledStmt) = {
    stmt.labels.map(_ + ":").mkString(" ") + "\t" + format(stmt.stmt)
  }

  def format(stmt: Stmt): String = stmt match {
    case Assign(assigns, constrain) =>
      val lhs = assigns.unzip._1.map(format).mkString(", ")
      val rhs = assigns.unzip._2.map(format).mkString(", ")
      val c = constrain.fold("")(e => " constrain " + format(e))
      s"$lhs := $rhs$c"
    case Assume(e)                  =>
      s"assume ${format(e)}"
    case Assert(e)                  =>
      s"assert ${format(e)}"
    case Call(name, assigns, args)  =>
      val lhs = if (assigns.isEmpty) {
        ""
      } else {
        assigns.map {
          case Some(Sym(name)) => name
          case None            => "_"
        }.mkString(" ", ", ", " := ")
      }
      val params = args.map(format).mkString(", ")
      s"""$lhs$name($params)"""
    case Dead(vars)                 =>
      s"""dead ${vars.mkString(", ")}"""
    case Goto(targets)              =>
      s"""goto ${targets.mkString(", ")}"""
    case If(condition, pos, neg)    =>
      val p = pos.map(s => format(s.stmt)).mkString("", end, end)
      val n = neg.map(s => format(s.stmt)).mkString("", end, end)
      val alt = if (neg.isEmpty) "" else s" else $n"
      s"""if ${format(condition)} then $p$alt fi"""
    case Skip                       =>
      "skip"
    case Return(values)             =>
      s"""return${if (values.isEmpty) "" else values.mkString(", ")}"""
    case AtomicBegin                =>
      "atomic_begin"
    case AtomicEnd                  =>
      "atomic_end"
    case StartThread(label)         =>
      s"start_thread goto $label"
    case EndThread                  =>
      "end_thread"
  }

  def format(e: Expr): String = {

    // what needs to be wrapped?
    // | inside & / CNF
    // ! before | or &

    // no wrapping:
    // ! before var
    // | or &'s / DNF

    def needsWrapping(operator: Expr, operand: Expr) = (operator, operand) match {
      case (NaryOp(And, _), NaryOp(Or, _))            => true // CNF
      case (_: Not, _: NaryOp | _: BinaryOp | _: Not) => true
      case _                                          => false
    }

    def wrapFormatted(operand: Expr) = {
      if (needsWrapping(e, operand)) {
        s"(${format(operand)})"
      } else {
        s"${format(operand)}"
      }
    }

    e match {
      case NaryOp(And, ops)            => ops.map(wrapFormatted).mkString(" & ")
      case NaryOp(Or, ops)             => ops.map(wrapFormatted).mkString(" | ")
      case BinaryOp(Impl, a, b)        => s"${wrapFormatted(a)} -> ${wrapFormatted(b)}"
      case BinaryOp(Xor, a, b)         => s"${wrapFormatted(a)} != ${wrapFormatted(b)}"
      case BinaryOp(Equiv, a, b)       => s"${wrapFormatted(a)} = ${wrapFormatted(b)}"
      case BinaryOp(Schoose, pos, neg) => s"schoose [${format(pos)}, ${format(neg)}]"
      case Not(a)                      => s"!${wrapFormatted(a)}"
      case True                        => "T"
      case False                       => "F"
      case Nondet                      => "*"
      case Var(sym, primed, mixed)     => primed + sym.name + mixed
    }
  }

  def writeToFile(program: Program, file: File) {
    val content = apply(program)
    FileUtils.writeLines(file, content.asJava)
  }
}
