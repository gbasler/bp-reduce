package bpReduce
package ast

import Expr._
import Expr.NaryOp
import Expr.Not
import Expr.BinaryOp

class ExprTraverser {

  /**
   * Traverses a single expr.
   * Classical visitor style in order to allow overriding in derived classes.
   */
  def traverse(e: Expr): Unit = e match {
    case Not(a)                  =>
      traverse(a)
    case BinaryOp(op, a, b)      =>
      traverse(a)
      traverse(b)
    case NaryOp(op, ops)         =>
      traverseTrees(ops)
    case True                    =>
    case False                   =>
    case Nondet                  =>
    case Var(sym, primed, mixed) =>
  }

  /** Traverses a list of exprs. */
  def traverseTrees(trees: Seq[Expr]) {
    trees foreach traverse
  }

  /** Traverses a list of lists of exprs. */
  def traverseTreess(treess: Seq[List[Expr]]) {
    treess foreach traverseTrees
  }
}
