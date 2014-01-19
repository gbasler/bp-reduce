package bpReduce.ast

import bpReduce.ast.Expr._
import bpReduce.ast.Expr.NaryOp
import bpReduce.ast.Expr.Not
import bpReduce.ast.Expr.BinaryOp

class Traverser {

  /** Traverses a single expr. */
  def traverse(e: Expr):Unit = e match {
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
