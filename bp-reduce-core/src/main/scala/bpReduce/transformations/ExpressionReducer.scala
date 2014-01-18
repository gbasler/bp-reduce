package bpReduce
package transformations

import bpReduce.ast.Expr
import bpReduce.ast.Expr._
import bpReduce.ast.Expr.Impl
import bpReduce.ast.Expr.Xor

class Monad[S, A](run: S => (S, A)) {
  def map[B](f: A => B): Monad[S, B] = new Monad({
    s: S =>
      val (s1, a) = run(s)
      s1 -> f(a)
  })
}

/**
 * Most essential reduction.
 *
 * E.g.
 *
 * a & b: a, b, T (a&b=T), F (a|b=F)
 * a | b: a, b, T (a|b=T), F (a&b=F)
 * a == b: (a & !b) | (!a & b): a | !a
 */
class ExpressionReducer {
  def reduce(e: Expr) = {

    // simplify expression (in order to have as few runs as possible)
    val simplified = ExpressionSimplifier(e)

    /**
     * Replace only one variable at a time because
     * others might become irrelevant
     * e.g., a | b: if we set a to true then b becomes don't care
     * so we won't have to check b
     */
    def replaceOneVarWithConsts(e: Expr, replace: Boolean): (Expr, Boolean) = {

      type S = Option[Expr]
      type TF = S => (Expr, S)

      class ReplaceMonad(val run: TF) {

        def map(f: Expr => Expr) = new ReplaceMonad({
          s: S =>
            val (e, s1) = run(s)
            f(e) -> s1
        })

        def flatMap(f: Expr => ReplaceMonad) = new ReplaceMonad({
          s: S =>
            val (e, s1) = run(s)
            f(e).run(s1)
        })
      }


      /*
       Blub.
        @param OhmeinGott
        */
      def replaceSeq(exprs: Seq[Expr], replacement: Option[Expr]): (Seq[Expr], Option[Expr]) = {
        ???
      }

      def replace1(e: Expr, replacement: Option[Expr]) = replacement match {
        case Some(x) => x -> None
        case None    => e -> None
      }

      def replace(e: Expr, replacement: Option[Expr]): (Expr, Option[Expr]) = {
        def shortCircuit(e1: Expr, r1: Option[Expr]) = {
          if (r1 == replacement)
            e -> replacement // short circuiting
          else
            e1 -> r1
        }

        // TODO: cleanup with extractor? binaryop, unaryop, ...
        e match {
          case NaryOp(op, ops)         =>
            val (r, e) = ops.foldLeft(replacement -> Seq.empty[Expr]) {
              case ((r, acc), expr) =>
                val (e1, r1) = replace(expr, r)
                (r1, acc :+ e1)
            }
            shortCircuit(NaryOp(op, e), r)
          case BinaryOp(op, a, b)      =>
            val (e1, r1) = replace(a, replacement)
            val (e2, r2) = replace(b, r1)
            shortCircuit(BinaryOp(op, e1, e2), r2)
          case Not(a)                  =>
            val (e1, r1) = replace(e, replacement)
            shortCircuit(Not(e1), r1)
          case True | False | Nondet   => e -> replacement
          case Var(sym, primed, mixed) => replacement.getOrElse(e) -> None
        }
      }
      ???
    }

  }

}
