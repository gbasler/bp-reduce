package bpReduce
package writer

import ast._
import bpReduce.ast.Stmt._
import bpReduce.ast.Stmt.Call
import bpReduce.ast.Stmt.Assume
import bpReduce.ast.VariableHolder
import bpReduce.ast.Stmt.Assign
import bpReduce.ast.Stmt.Assert
import bpReduce.ast.Program
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

object Formatter {

  private val end = ";"

  def format(program: Program) = {
    val globals = format(program.globals)
    val functions = for {
      f <- program.functions
    } yield {
      val locals = format(f.locals)
      val returns = if (f.returns == 0) "void" else s"bool<${f.returns}>"
      val args = f.args.mkString(", ")
      val header = s"$returns ${f.name}($args)"
      Seq(header, "begin") ++ f.stmts.map(format) :+ "end"
    }
  }

  def format(vars: VariableHolder): String = {
    {
      for {
        v <- vars.vars
      } yield {
        v.name
      }
    }.mkString("decl ", ", ", end)
  }

  def format(stmt: Stmt): String = stmt match {
    case Assign(assigns, constrain) =>
      s"""${assigns.unzip._1.map(format).mkString(",")} := ${assigns.unzip._2.map(format).mkString(",")}"""
    case Assume(e)                  =>
      s"assume ${format(e)}"
    case Assert(e)                  =>
      s"assert ${format(e)}"
    case Call(name, assigns, args)  =>
    case Dead(vars)                 =>
      s"""dead ${vars.mkString(", ")}"""
    case Goto(targets)              =>
      s"""goto ${targets.mkString(", ")}"""
    case If(condition, pos, neg)    =>
      s"""if ${format(condition)} then ${pos.map(format).mkString(end)} else ${neg.map(format).mkString(end)} fi"""
    case Skip                       =>
      "skip"
    case Return(values)             =>
      s"""return ${values.mkString(", ")}"""
    case AtomicBegin                =>
      "atomic_begin"
    case AtomicEnd                  =>
      "atomic_end"
    case StartThread(label)         =>
      s"start_thread goto $label"
    case EndThread                  =>
      "end_thread"
  }

  def format(e: Expr) = e match {
    case And(a, b)               => s"(${format(a)}) & (${format(b)})"
    case Or(a, b)                => s"(${format(a)}) | (${format(b)})"
    case Impl(a, b)              => s"(${format(a)}) -> (${format(b)})"
    case Xor(a, b)               => s"(${format(a)}) != (${format(b)})"
    case Equiv(a, b)             => s"(${format(a)}) = (${format(b)})"
    case Schoose(pos, neg)       => s"schoose [${format(pos)}, ${format(neg)}]"
    case Not(a)                  => s"!(${format(a)})"
    case True                    => "T"
    case False                   => "F"
    case Nondet                  => "*"
    case Var(sym, primed, mixed) => s"""${if (primed) "$" else ""}${sym.name}${if (mixed) "$" else ""}"""
  }
}
