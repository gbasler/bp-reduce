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

object Formatter {

  private val end = ";"

  def format(program: Program): String = {
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
    stmt.labels.mkString(" ") + "\t" + format(stmt.stmt)
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
      val lhs = assigns.map {
        case Some(Sym(name)) => name
        case None            => "_"
      }.mkString(" ", ", ", " := ")
      val params = args.map(format)
      s"""$lhs$name($params)"""
    case Dead(vars)                 =>
      s"""dead ${vars.mkString(", ")}"""
    case Goto(targets)              =>
      s"""goto ${targets.mkString(", ")}"""
    case If(condition, pos, neg)    =>
      s"""if ${format(condition)} then ${pos.map(s => format(s.stmt)).mkString(end)} else ${neg.map(s => format(s.stmt)).mkString(end)} fi"""
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

  def format(e: Expr): String = {

    // what needs to be wrapped?
    // | inside & / CNF
    // ! before | or &

    // no wrapping:
    // ! before var
    // | or &'s / DNF

    def needsWrapping(operator: Expr, operand: Expr) = (operator, operand) match {
      case (_: And, _: Or)          => true // CNF
      case (_: Not, _: And | _: Or) => true
      case (_: Not, _Not)           => true
      case _                        => false
    }

    def wrapFormatted(operand: Expr) = {
      if (needsWrapping(e, operand)) {
        s"(${format(operand)})"
      } else {
        s"${format(operand)}"
      }
    }

    e match {
      case And(a, b)               => s"${wrapFormatted(a)} & ${wrapFormatted(b)}"
      case Or(a, b)                => s"${wrapFormatted(a)} | ${wrapFormatted(b)}"
      case Impl(a, b)              => s"${wrapFormatted(a)} -> ${wrapFormatted(b)}"
      case Xor(a, b)               => s"${wrapFormatted(a)} != ${wrapFormatted(b)}"
      case Equiv(a, b)             => s"${wrapFormatted(a)} = ${wrapFormatted(b)}"
      case Schoose(pos, neg)       => s"schoose [${format(pos)}, ${format(neg)}]"
      case Not(a)                  => s"!${wrapFormatted(a)}"
      case True                    => "T"
      case False                   => "F"
      case Nondet                  => "*"
      case Var(sym, primed, mixed) => primed + sym.name + mixed
    }
  }
}
