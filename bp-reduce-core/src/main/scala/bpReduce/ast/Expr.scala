package bpReduce
package ast

import scala.collection

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

  final case class And(ops: Seq[Expr]) extends Expr

  object And {
    def apply(a: Expr, b: Expr) = new And(Seq(a, b))

    //    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
    //      case And(a, b) => Some(a -> b)
    //      case _         => None
    //    }
  }

  object AndSeq {
    def unapply(e: Expr): Option[Seq[Expr]] = e match {
      case And(ops) => Some(ops)
      case _        => None
    }
  }

  final case class Or(ops: Seq[Expr]) extends Expr

  object Or {
    def apply(a: Expr, b: Expr) = new Or(Seq(a, b))

//    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
//      case Or(a, b) => Some(a -> b)
//      case _        => None
//    }
  }

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