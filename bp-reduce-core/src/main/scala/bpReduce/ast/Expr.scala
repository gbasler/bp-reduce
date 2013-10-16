package bpReduce.ast

sealed abstract class StateIdentifier

object StateIdentifier {

  /**
   * E.g., {{{g}}}
   */
  case object Current extends StateIdentifier

  /**
   * E.g., {{{'g}}}
   */
  case object Next extends StateIdentifier

}

sealed abstract class MixedIdentifier

object MixedIdentifier {

  /**
   * E.g., {{{$l}}}
   */
  case object Mixed extends MixedIdentifier

  /**
   * E.g., {{{l}}}
   */
  case object NonMixed extends MixedIdentifier

}

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

  final case class Var(name: Sym, primed: Boolean = false, mixed: Boolean = false) extends Expr

}