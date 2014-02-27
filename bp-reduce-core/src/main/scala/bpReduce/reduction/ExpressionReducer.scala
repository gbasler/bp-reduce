package bpReduce
package reduction

import bpReduce.ast.Expr
import bpReduce.ast.Expr._
import bpReduce.ast.Expr.NaryOp
import bpReduce.ast.Expr.BinaryOp
import bpReduce.ast.Expr.Var
import bpReduce.ast.Expr.Not
import bpReduce.transformations.ExpressionSimplifier

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

  // TODO: List is more elegant...
  def apply(e: Expr): Set[Expr] = {

    /**
     * We use a power set approach here:
     * Given an expression, collect all variables that are present in the formula.
     * The first round of reduction, removes just one variable, then a second one is removed and so on until
     * we have removed all variables (max reduction).
     * However we need all possible orderings. Consider we can remove `a` or `b`. In order to arrive at the state
     * where both are removed we can remove `a` first and then `b` or vice-versa, so we need to explore both orderings.
     * That is were the decrementing power set algorithm comes into play: it allows a nice recursive implementation
     * with a simple work list.
     */
    def replaceAllVarsOnce(e: Expr): Set[Expr] = {

      def collectVars(e: Expr) = e.collect {
        case v: Var => v
      }.toSet

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
          case Not(e)                =>
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

      def decrementalPowerSet(expr: Expr): Set[Expr] = {
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
      decrementalPowerSet(e)
    }

    def expandNondets(e: Expr): Set[Expr] = {

      /**
       * Unlike variables, where you'd expect all occurrences of the same
       * variable have the same value, a nondet is different from any other nondet.
       * However, since they are represented with the same constant,
       */
      def replaceNondetOnce(e: Expr,
                            counter: Int,
                            replacement: Expr): Expr = {

        def replace(e: Expr,
                    counter: Int): (Expr, Int) = {

          e match {
            case NaryOp(op, ops)       =>
              val (c, e) = ops.foldLeft(counter -> Seq.empty[Expr]) {
                case ((c, acc), expr) =>
                  val (e1, c1) = replace(expr, c)
                  (c1, acc :+ e1)
              }
              NaryOp(op, e) -> c
            case BinaryOp(op, a, b)    =>
              val (e1, c1) = replace(a, counter)
              val (e2, c2) = replace(b, c1)
              BinaryOp(op, e1, e2) -> c2
            case Not(a)                =>
              val (e1, c1) = replace(a, counter)
              Not(e1) -> c1
            case True | False | _: Var =>
              e -> counter
            case Nondet                =>
              if (counter == 0) {
                (replacement, counter - 1)
              } else {
                (e, counter - 1)
              }
          }
        }

        replace(e, counter)._1
      }

      val nondets: Int = e.count {
        case Nondet => true
        case _      => false
      }

      (0 until nondets).flatMap {
        counter =>
          Seq(replaceNondetOnce(e, counter, True),
            replaceNondetOnce(e, counter, False))
      }(collection.breakOut)
    }

    // simplify expression (in order to have as few runs as possible)
    val simplified = ExpressionSimplifier(e)

    def hasNondets(e: Expr) = {
      val nondets = e.count {
        case Nondet => true
        case _      => false
      }
      nondets > 0
    }

    // expand nondet variables but only if there are any
    val reduced = for {
      replacedVars <- replaceAllVarsOnce(simplified)
      expandedNondets <- if (hasNondets(replacedVars)) expandNondets(replacedVars) + replacedVars else Set(replacedVars)
    } yield expandedNondets

    val result = if (reduced.isEmpty && hasNondets(simplified)) {
      // we did not find a reduction
      // however there might be the possibility of nondet variables but no regular variables
      expandNondets(simplified)
    } else {
      reduced
    }
    result.map(ExpressionSimplifier(_))
  }

}
