package bpReduce
package transformations

import bpReduce.ast.{ExprTransformer, Expr}
import bpReduce.ast.Expr._
import bpReduce.ast.Expr.NaryOp
import bpReduce.ast.Expr.BinaryOp
import bpReduce.ast.Expr.Var
import bpReduce.ast.Expr.Not
import scala.collection.immutable.Nil

/**
 * Most essential reduction.
 *
 * E.g.
 *
 * a & b: a, b, T (a&b=T), F (a|b=F)
 * a | b: a, b, T (a|b=T), F (a&b=F)
 * a == b: (a & !b) | (!a & b): a | !a
 *
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

    def replaceAllVarsOnce(e: Expr,
                           vars: List[Var]): Seq[Expr] = vars match {
      case Nil     =>
        Nil
      case v :: tl =>
        val (withTrue, replaced) = replace(e, Some(v -> True))
        val withFalse = replace(e, Some(v -> False))._1

        if (replaced.isDefined) {
          // not replaced
          Seq()
        } else {
          Seq(withTrue, withFalse) ++
            replaceAllVarsOnce(withTrue, tl) ++
            replaceAllVarsOnce(withFalse, tl)
        }
    }

    // simplify expression (in order to have as few runs as possible)
    val simplified = ExpressionSimplifier(e)

    val vars = simplified.collect {
      case v: Var => v
    }.distinct

    val replaced = replaceAllVarsOnce(simplified, vars)
    replaced.map(ExpressionSimplifier(_)).toSet
  }

}
