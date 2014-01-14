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


      def replaceSeq(exprs: Seq[Expr], replacement: Option[Expr]): (Seq[Expr], Option[Expr]) = {
        ???
      }

      /**
       * @param replacement constant (T/F) to use to replace first variables
       */
      def replace(e: Expr, replacement: Option[Expr]): (Expr, Option[Expr]) = e match {
        case And(ops)                =>
          val (os, replaced) = replaceSeq(ops, replacement)
          And(os) -> replaced
        case Or(ops)                 =>
          val (os, replaced) = replaceSeq(ops, replacement)
          Or(os) -> replaced
        case Impl(a, b)              =>
          for {
            a0 <- replace(a, replacement)
            b0 <- replace(a, r0)
          } yield {
            Impl(a0, b0)
          }

          val (a0, r0) = replace(a, replacement)
          val (b0, r1) = replace(a, r0)
          Impl(a0, b0) -> r1
        case Xor(a, b)               =>
        case Equiv(a, b)             =>
        case Schoose(pos, neg)       =>
        case Not(a)                  =>
        case True                    =>
        case False                   =>
        case Nondet                  =>
        case Var(sym, primed, mixed) =>
      }

      //      e match {
      //        case And(ops) =>
      //          val a: (Seq[Expr], Boolean) = ops.foldLeft((Seq.empty[Expr], replace)) {
      //                      case ((acc, true), expr) =>
      //              acc -> true
      //          }

      //          val o = ops.flatMap {
      //            collectVars(_, replace)
      //          }
      //        case Or(ops)                 =>
      //          ops.flatMap(collectVars)
      //        case Impl(a, b)              =>
      //          Seq(a, b).flatMap(collectVars)
      //        case Xor(a, b)               =>
      //          Seq(a, b).flatMap(collectVars)
      //        case Equiv(a, b)             =>
      //          Seq(a, b).flatMap(collectVars)
      //        case Schoose(pos, neg)       =>
      //          Seq(pos, neg).flatMap(collectVars)
      //        case Not(a)                  =>
      //          collectVars(a)
      //        case True | False | Nondet   =>
      //          Seq()
      //        case Var(sym, primed, mixed) =>
      //          Seq(sym)
      //    }

    }

    //    // collect all leaves (variables)
    //    def collectVars(e: Expr): Seq[Expr] = {
    //      e match {
    //        case And(ops)                =>
    //          ops.flatMap(collectVars)
    //        case Or(ops)                 =>
    //          ops.flatMap(collectVars)
    //        case Impl(a, b)              =>
    //          Seq(a, b).flatMap(collectVars)
    //        case Xor(a, b)               =>
    //          Seq(a, b).flatMap(collectVars)
    //        case Equiv(a, b)             =>
    //          Seq(a, b).flatMap(collectVars)
    //        case Schoose(pos, neg)       =>
    //          Seq(pos, neg).flatMap(collectVars)
    //        case Not(a)                  =>
    //          collectVars(a)
    //        case True | False | Nondet   =>
    //          Seq()
    //        case Var(sym, primed, mixed) =>
    //          Seq(sym)
    //      }
    //    }
    //    val leaves =

  }

}
