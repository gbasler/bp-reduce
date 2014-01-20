package bpReduce
package ast

import scala.collection.mutable.ListBuffer
import Expr._

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

sealed abstract class Expr {
  /**
   * @param pf Applied to each expr on which the function is defined and collect the results.
   */
  def collect[T](pf: PartialFunction[Expr, T]): List[T] = {
    val results = new ListBuffer[T]

    new ExprTraverser {
      override def traverse(e: Expr) {
        if (pf.isDefinedAt(e)) results += pf(e)
        super.traverse(e)
      }
    }.traverse(this)

    results.toList
  }

  /**
   * @param pf Applied to each expr on which the function is defined and collect the results.
   */
  def transform(pf: PartialFunction[Expr, Expr]): Expr = {
    new ExprTransformer {
      override def transform(e: Expr): Expr = {
        if (pf.isDefinedAt(e)) {
          pf(super.transform(e))
        } else {
          super.transform(e)
        }
      }
    }.transform(this)
  }
}

object Expr {

  sealed abstract class BinOp

  case object Impl extends BinOp {
    def apply(a: Expr, b: Expr) = new BinaryOp(Impl, a, b)

    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case BinaryOp(Impl, a, b) => Some(a -> b)
      case _                    => None
    }
  }

  case object Xor extends BinOp {
    def apply(a: Expr, b: Expr) = new BinaryOp(Xor, a, b)

    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case BinaryOp(Xor, a, b) => Some(a -> b)
      case _                   => None
    }
  }

  case object Equiv extends BinOp {
    def apply(a: Expr, b: Expr) = new BinaryOp(Equiv, a, b)

    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case BinaryOp(Equiv, a, b) => Some(a -> b)
      case _                     => None
    }
  }

  case object Schoose extends BinOp {
    def apply(pos: Expr, neg: Expr) = new BinaryOp(Schoose, pos, neg)

    def unapply(e: Expr): Option[(Expr, Expr)] = e match {
      case BinaryOp(Schoose, a, b) => Some(a -> b)
      case _                       => None
    }
  }


  sealed abstract class NOp

  case object And extends NOp {
    def apply(a: Expr, b: Expr) = new NaryOp(And, Seq(a, b))

    def apply(ops: Expr*) = new NaryOp(And, ops)

    def unapply(e: Expr): Option[Seq[Expr]] = e match {
      case NaryOp(And, ops) => Some(ops)
      case _                => None
    }
  }

  case object Or extends NOp {
    def apply(a: Expr, b: Expr) = new NaryOp(Or, Seq(a, b))

    def apply(ops: Expr*) = new NaryOp(Or, ops)

    def unapply(e: Expr): Option[Seq[Expr]] = e match {
      case NaryOp(Or, ops) => Some(ops)
      case _               => None
    }
  }

  final case class Not(a: Expr) extends Expr

  /**
   * We use a two stage approach here: instead of having explicit Xor, Equiv, etc
   * we have an operation and two operands.
   * This makes it very easy to recreate an expression tree while traversing, since
   * only one case must be considered instead of 4.
   */
  final case class BinaryOp(op: BinOp, a: Expr, b: Expr) extends Expr

  /**
   * And / Or. Explanation for design choice can be read in [[bpReduce.ast.Expr.BinaryOp]]]
   */
  final case class NaryOp(op: NOp, ops: Seq[Expr]) extends Expr

  case object True extends Expr

  case object False extends Expr

  case object Nondet extends Expr

  import StateIdentifier._
  import MixedIdentifier._

  final case class Var(sym: Sym, primed: StateIdentifier = Current, mixed: MixedIdentifier = NonMixed) extends Expr {
    override def toString = s"$primed$mixed${sym.name}"
  }

}