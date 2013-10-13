package bpReduce.ast


sealed abstract class Expr

object Expr {

  final case class And(a: Expr, b: Expr) extends Expr

  final case class Or(a: Expr, b: Expr) extends Expr

  final case class Impl(a: Expr, b: Expr) extends Expr

  final case class Xor(a: Expr, b: Expr) extends Expr

  final case class Equiv(a: Expr, b: Expr) extends Expr

  final case class Schoose(pos: Expr, neg: Expr) extends Expr

  final case class Not(a: Expr) extends Expr

  case object True extends Expr

  case object False extends Expr

  case object Nondet extends Expr

  final case class Var(name: Sym, primed: Boolean = false) extends Expr

}