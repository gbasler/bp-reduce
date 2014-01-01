package bpReduce
package ast

sealed abstract class StateIdentifier

object StateIdentifier {

  /**
   * E.g., {{{g}}}
   */
  case object Current extends StateIdentifier {
    override def toString = ""
  }

  /**
   * E.g., {{{'g}}}
   */
  case object Next extends StateIdentifier {
    override def toString = "'"
  }

}

sealed abstract class MixedIdentifier

object MixedIdentifier {

  /**
   * E.g., {{{$l}}}
   */
  case object Mixed extends MixedIdentifier {
    override def toString = "$"
  }

  /**
   * E.g., {{{l}}}
   */
  case object NonMixed extends MixedIdentifier {
    override def toString = ""
  }

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

  import StateIdentifier._
  import MixedIdentifier._
  final case class Var(sym: Sym, primed: StateIdentifier = Current, mixed: MixedIdentifier = NonMixed) extends Expr {
    override def toString = s"$primed$mixed${sym.name}"
  }

}