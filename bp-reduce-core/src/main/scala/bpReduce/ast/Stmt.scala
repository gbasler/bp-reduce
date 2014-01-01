package bpReduce
package ast

import bpReduce.ast.Expr.Var

sealed abstract class Stmt

object Stmt {

  final case class Assign(assigns: Seq[(Var, Expr)], constrain: Option[Expr]) extends Stmt

  final case class Assume(e: Expr) extends Stmt

  final case class Assert(e: Expr) extends Stmt

  final case class Call(name: String, assigns: Seq[Option[Sym]], args: Seq[Expr]) extends Stmt

  final case class Dead(vars: Seq[Sym]) extends Stmt

  final case class Goto(targets: Seq[String]) extends Stmt

  final case class If(condition: Expr, pos: Seq[Stmt], neg: Seq[Stmt]) extends Stmt

  case object Skip extends Stmt

  final case class Return(values: Seq[Expr]) extends Stmt

  case object AtomicBegin extends Stmt

  case object AtomicEnd extends Stmt

  final case class StartThread(label: String) extends Stmt

  case object EndThread extends Stmt

}

