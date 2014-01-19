package bpReduce
package ast

import bpReduce.ast.Expr._
import bpReduce.ast.Expr.NaryOp
import bpReduce.ast.Expr.BinaryOp
import bpReduce.ast.Expr.Not

class ExprTransformer {
  def transform(e: Expr): Expr = e match {
    case Not(a)                         =>
      Not(transform(e))
    case BinaryOp(op, a, b)             =>
      BinaryOp(op, transform(a), transform(b))
    case NaryOp(op, ops)                =>
      NaryOp(op, transformTrees(ops.toList))
    case _: Var | True | False | Nondet =>
      e
  }

  /** Transforms a list of trees. */
  def transformTrees(trees: List[Expr]): List[Expr] = trees mapConserve transform

}
