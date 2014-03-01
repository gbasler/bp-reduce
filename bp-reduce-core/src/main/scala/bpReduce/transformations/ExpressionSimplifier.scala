package bpReduce
package transformations

import scala.annotation.tailrec
import ast.Expr
import ast.Expr._
import ast.Expr.NaryOp
import ast.Expr.And
import ast.Expr.Not
import ast.Expr.Var

/**
 * Simplifies Boolean Expression according to the following rules:
 * - eliminate double negation
 * - flatten trees of same connectives
 * - removes constants and connectives that are in fact constant because of their operands
 * - eliminates duplicated operands
 */
object ExpressionSimplifier {

  def apply(f: Expr): Expr = {
    def hasImpureAtom(ops: Seq[Expr]): Boolean = ops.combinations(2).exists {
      case Seq(a, Not(b)) if a == b => true
      case Seq(Not(a), b) if a == b => true
      case _                        => false
    }

    @tailrec
    def isAtom(e: Expr): Boolean = e match {
      case _: Var | True | False => true
      case Not(a)                => isAtom(a)
      case _                     => false
    }

    def simplify(e: Expr) = e match {
      case NaryOp(op, fv)                              =>

        // value that, if one of the operands is set to this value
        // the output is set to that value irrespective of the other inputs
        val drivingValue = op match {
          case And => False
          case Or  => True
        }

        // value to use if number of operands is zero
        val emptyValue = op match {
          case And => True
          case Or  => False
        }

        val ops = fv.map(apply).distinct.filterNot(_ == emptyValue)
        if (ops.exists(_ == drivingValue)) {
          drivingValue
        } else {
          val opsFlattened = ops.flatMap {
            case NaryOp(`op`, fv) => fv
            case f                => Seq(f)
          }

          if (hasImpureAtom(opsFlattened)) {
            drivingValue
          } else {
            opsFlattened match {
              case Seq()  => emptyValue
              case Seq(f) => f
              case ops    => NaryOp(op, ops)
            }
          }
        }
      case Not(True)                                   =>
        False
      case Not(False)                                  =>
        True
      case Not(Not(op))                                =>
        apply(op)
      case Not(op)                                     =>
        Not(apply(op))
      case Not(NaryOp(And, ops)) if ops.forall(isAtom) =>
        // use De Morgan's rule to push negation into operands
        // (might allow flattening of tree of connectives closer to root)
        apply(NaryOp(Or, ops.map(Not)))
      case Not(NaryOp(Or, ops)) if ops.forall(isAtom)  =>
        // De Morgan
        apply(NaryOp(And, ops.map(Not)))
      case Equiv(a, b)                                 =>
        val a0 = apply(a)
        val b0 = apply(b)
        (a0, b0) match {
          case (Nondet, _) | (_, Nondet) => Nondet
          case (x, y) if x == y          => True
          case (o, True)                 => o
          case (o, False)                => apply(Not(o))
          case (True, o)                 => o
          case (False, o)                => apply(Not(o))
          case _                         => Equiv(a0, b0)
        }
      case Xor(a, b)                                   =>
        val a0 = apply(a)
        val b0 = apply(b)
        (a0, b0) match {
          case (Nondet, _) | (_, Nondet) => Nondet
          case (x, y) if x == y          => False
          case (o, True)                 => apply(Not(o))
          case (o, False)                => o
          case (True, o)                 => apply(Not(o))
          case (False, o)                => o
          case _                         => Xor(a0, b0)
        }
      case p                                           => p
    }

    @tailrec
    def simplifyFP(e: Expr): Expr = {
      val simplified = simplify(e)
      if (simplified != e) {
        simplifyFP(simplified)
      } else {
        simplified
      }
    }

    simplifyFP(f)
  }
}
