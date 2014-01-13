package bpReduce
package transformations

import scala.annotation.tailrec
import bpReduce.ast.Expr
import bpReduce.ast.Expr._
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
      case And(fv)                             =>
        val ops = fv.map(apply).distinct.filterNot(_ == True)
        if (ops.exists(_ == False)) {
          False
        } else {
          val opsFlattened = ops.flatMap {
            case And(fv) => fv
            case f       => Seq(f)
          }

          if (hasImpureAtom(opsFlattened)) {
            False
          } else {
            opsFlattened match {
              case Seq()  => True
              case Seq(f) => f
              case ops    => And(ops)
            }
          }
        }
      case Or(fv)                              =>
        val ops = fv.map(apply).distinct.filterNot(_ == False)
        if (ops.exists(_ == True)) {
          True
        } else {
          val opsFlattened = ops.flatMap {
            case Or(fv) => fv
            case f      => Seq(f)
          }

          if (hasImpureAtom(opsFlattened)) {
            True
          } else {
            opsFlattened match {
              case Seq()  => False
              case Seq(f) => f
              case ops    => Or(ops)
            }
          }
        }
      case Not(Not(a))                         =>
        apply(a)
      case Not(And(ops)) if ops.forall(isAtom) =>
        // use De Morgan's rule to push negation into operands
        // (might allow flattening of tree of connectives closer to root)
        apply(Or(ops.map(Not)))
      case Not(Or(ops)) if ops.forall(isAtom)  =>
        // De Morgan
        apply(And(ops.map(Not)))
      case p                                   => p
    }
  }
}
