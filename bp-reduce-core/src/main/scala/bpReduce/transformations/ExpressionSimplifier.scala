package bpReduce
package transformations

import scala.annotation.tailrec
import bpReduce.ast.Expr
import bpReduce.ast.Expr._
import bpReduce.ast.Expr.NaryOp
import bpReduce.ast.Expr.And
import bpReduce.ast.Expr.Not
import bpReduce.ast.Expr.Var

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
    def isAtom(f: Expr): Boolean = f match {
      case _: Var | True | False => true
      case Not(a)                => isAtom(a)
      case _                     => false
    }

    f match {
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

        val ops = fv.map(apply).distinct.filterNot(_ == True)
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
      case Not(Not(a))                                 =>
        apply(a)
      case Not(NaryOp(And, ops)) if ops.forall(isAtom) =>
        // use De Morgan's rule to push negation into operands
        // (might allow flattening of tree of connectives closer to root)
        apply(NaryOp(Or, ops.map(Not)))
      case Not(NaryOp(Or, ops)) if ops.forall(isAtom)  =>
        // De Morgan
        apply(NaryOp(And, ops.map(Not)))
      case p                                           => p
    }
  }
}
