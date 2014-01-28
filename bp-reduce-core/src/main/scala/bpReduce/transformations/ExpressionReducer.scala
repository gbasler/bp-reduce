package bpReduce
package transformations

import bpReduce.ast.{ExprTransformer, Expr}
import bpReduce.ast.Expr._
import bpReduce.ast.Expr.NaryOp
import bpReduce.ast.Expr.BinaryOp
import bpReduce.ast.Expr.Var
import bpReduce.ast.Expr.Not
import scala.annotation.tailrec

/**
 * Most essential reduction.
 *
 * E.g.
 *
 * a & b: a, b, T (a&b=T), F (a|b=F)
 * a | b: a, b, T (a|b=T), F (a&b=F)
 * a == b: (a & !b) | (!a & b): a | !a
 *
 * Algorithm
 * - collect all leaves (vars)
 * - create powerset of expression with elements (vars) of powerset set to T/F
 * since we can only remove but not add elements
 *
 * TODO:
 * How are the reductions ordered? The expression reduction forms a lattice (from the powersets),
 * so we could
 * 1) move along the leaves until a reduction is successful or all leaves are tested
 * 2) from that leave, we continue towards the root (T/F) until we reach it or a reduction is unsuccessful
 * => this works for one expression but we need a whole program transformation...
 * when are the other expressions updated?
 *
 * If we deliver programs one for one, it's enough to calculate the next more reduced expression upwards in the lattice.
 */
object ExpressionReducer {

  def apply(e: Expr): Set[Expr] = {

    // TODO: write correct transformer
    class ReduceTransformer() extends ExprTransformer {
      override def transform(e: Expr): Expr = {
        super.transform(e)
      }
    }

    /**
     * Replace only one variable at a time because
     * others might become irrelevant
     * e.g., a | b: if we set a to true then b becomes don't care
     * so we won't have to check b
     */
    def replace(e: Expr,
                replacement: Option[(Expr, Expr)]): (Expr, Option[(Expr, Expr)]) = {

      val (reducedExpr, replaced) = e match {
        case NaryOp(op, ops)       =>
          val (r, e) = ops.foldLeft(replacement -> Seq.empty[Expr]) {
            case ((r, acc), expr) =>
              val (e1, r1) = replace(expr, r)
              (r1, acc :+ e1)
          }
          NaryOp(op, e) -> r
        case BinaryOp(op, a, b)    =>
          val (e1, r1) = replace(a, replacement)
          val (e2, r2) = replace(b, r1)
          BinaryOp(op, e1, e2) -> r2
        case Not(a)                =>
          val (e1, r1) = replace(e, replacement)
          Not(e1) -> r1
        case True | False | Nondet =>
          e -> replacement
        case v: Var                =>
          replacement match {
            case Some((from, to)) if from == v =>
              to -> None
            case _                             =>
              v -> replacement
          }
      }

      // mapConserve!!!
      def shortCircuit(e1: Expr, r1: Option[(Expr, Expr)]) = {
        if (r1 == replacement)
          e -> replacement // short circuiting
        else
          e1 -> r1
      }

      shortCircuit(reducedExpr, replaced)
    }

    def collectVars(e: Expr) = e.collect {
      case v: Var => v
    }.toSet


    def replaceAllVarsOnce(e: Expr): Set[Expr] = {
      @tailrec
      def decrementalPowerSet(wl: Set[Expr],
                              acc: Set[Expr]): Set[Expr] = {
        if (wl.nonEmpty) {
          val next = wl.flatMap {
            expr =>
              val vars = collectVars(expr)
              vars.flatMap {
                v =>
                  val (withTrue, replaced) = replace(expr, Some(v -> True))
                  val withFalse = replace(expr, Some(v -> False))._1

                  if (replaced.isDefined) {
                    // not replaced
                    Set.empty[Expr]
                  } else {
                    Set(ExpressionSimplifier(withTrue), ExpressionSimplifier(withFalse))
                  }
              }
          }
          decrementalPowerSet(next, acc ++ next)
        } else {
          acc
        }
      }
      decrementalPowerSet(Set(e), Set())
    }

    // simplify expression (in order to have as few runs as possible)
    val simplified = ExpressionSimplifier(e)

    val replaced = replaceAllVarsOnce(simplified)
    replaced
  }

}
