package bpReduce.ast

sealed abstract class Stmt

object Stmt {

  final case class Assign() extends Stmt

  final case class Assume(e: Expr) extends Stmt

  final case class Assert(e: Expr) extends Stmt

  final case class Call(name: String, args: List[Expr]) extends Stmt

  final case class Dead(vars: List[String]) extends Stmt

  final case class Goto(targets: List[String]) extends Stmt

  final case class If(condition: Expr, pos: List[String], neg: List[String]) extends Stmt

  case object Skip extends Stmt

  final case class Return(values: List[Expr]) extends Stmt

  case object AtomicBegin extends Stmt

  case object AtomicEnd extends Stmt

  final case class StartThread(label: String) extends Stmt

  case object EndThread extends Stmt

}

