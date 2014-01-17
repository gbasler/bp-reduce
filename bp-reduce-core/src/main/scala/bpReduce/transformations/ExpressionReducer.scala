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

      class ReplaceMonad(run: TF) {

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

        object BinaryOp {
          def unapply(e: Expr): Option[((Expr, Expr), (Expr, Expr) => Expr)] = e match {
            case Impl(a, b)        => Some((a, b) -> {
              (a: Expr, b: Expr) => Impl(a, b)
            })
            case Xor(a, b)         => Some((a, b) -> {
              (a: Expr, b: Expr) => Xor(a, b)
            })
            case Equiv(a, b)       => Some((a, b) -> {
              (a: Expr, b: Expr) => Equiv(a, b)
            })
            case Schoose(pos, neg) => Some((pos, neg) -> {
              (a: Expr, b: Expr) => Schoose(a, b)
            })
            case _                 => None
          }
        }

        // TODO: cleanup with extractor? binaryop, unaryop, ...
        e match {
          case And(ops)                =>
            val (r, e) = ops.foldLeft(replacement -> Seq.empty[Expr]) {
              case ((r, acc), expr) =>
                val (e1, r1) = replace(expr, r)
                (r1, acc :+ e1)
            }
            shortCircuit(And(e), r)
          case Or(ops)                 =>
            val (r, e) = ops.foldLeft(replacement -> Seq.empty[Expr]) {
              case ((r, acc), expr) =>
                val (e1, r1) = replace(expr, r)
                (r1, acc :+ e1)
            }
            shortCircuit(Or(e), r)
          case Impl(a, b)              =>
            val (e1, r1) = replace(a, replacement)
            val (e2, r2) = replace(b, r1)
            shortCircuit(Impl(e1, e2), r2)
          case Xor(a, b)               =>
            val (e1, r1) = replace(a, replacement)
            val (e2, r2) = replace(b, r1)
            shortCircuit(Xor(e1, e2), r2)
          case Equiv(a, b)             =>
            val (e1, r1) = replace(a, replacement)
            val (e2, r2) = replace(b, r1)
            shortCircuit(Equiv(e1, e2), r2)
          case Schoose(pos, neg)       =>
            val (e1, r1) = replace(pos, replacement)
            val (e2, r2) = replace(pos, r1)
            shortCircuit(Schoose(e1, e2), r2)
          case Not(a)                  =>
            val (e1, r1) = replace(e, replacement)
            shortCircuit(Not(e1), r1)
          case True | False | Nondet   => e -> replacement
          case Var(sym, primed, mixed) => replacement.getOrElse(e) -> None
        }
      }

      /**
       * @param replacement constant (T/F) to use to replace first variables
       */
      def replace(e: Expr, m: ReplaceMonad): (Expr, ReplaceMonad) = e match {
        case And(ops)          =>
          val (os, replaced) = replaceSeq(ops, replacement)
          And(os) -> replaced
        case Or(ops)           =>
          val (os, replaced) = replaceSeq(ops, replacement)
          Or(os) -> replaced
        case Impl(a, b)        =>
          val a: ReplaceMonad = for {
            a0 <- m
            b0 <- m
          } yield {
            Impl(a0, b0)
          }
        case Xor(a, b)         =>
          val a: ReplaceMonad = (m).flatMap {
            case a0 => (m).map {
              case b0 => {
                Xor(a0, b0)
              }
            }
          }
        case Equiv(a, b)       =>
          val a: ReplaceMonad = for {
            a0 <- m
            b0 <- m
          } yield {
            Equiv(a0, b0)
          }
        case Schoose(pos, neg) =>
          val a: ReplaceMonad = for {
            a0 <- m
            b0 <- m
          } yield {
            Schoose(a0, b0)
          }

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
