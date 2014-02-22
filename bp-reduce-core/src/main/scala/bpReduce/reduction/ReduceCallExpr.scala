package bpReduce
package reduction

import bpReduce.ast.Stmt
import bpReduce.ast.Stmt.Call

/**
 * Reduces `v1, v2, ... := call f(e1, e2, ... en)` expressions by incrementally reducing the expressions.
 *
 * The algorithm reduces only one part of the formals or only one part
 * of the returned values per iteration.
 *
 * TODO: the lhs is not reduced for now... (e.g., replacing a variable with just `_`)
 *
 * @param next The next possible reduction for [[StmtReducer.advance]].
 */
final case class ReduceCallExpr(from: Call,
                                next: List[Stmt]) extends StmtReducer {

  require(next.nonEmpty)

  def to: Stmt = next.head

  def reduce: Option[ReduceCallExpr] = {
    ReduceCallExpr(to)
  }

  def advance: Option[ReduceCallExpr] = {
    // previous reduction not successful:
    // try next reduction
    next match {
      case _ :: Nil     => None
      case head :: tail => Some(copy(next = tail))
    }
  }
}

object ReduceCallExpr {
  def apply(stmt: Stmt): Option[ReduceCallExpr] = {

    stmt match {
      case call: Call => apply(call)
      case _          => None
    }
  }

  def apply(call: Call): Option[ReduceCallExpr] = {

    val reductions = call.args match {
      case Seq(expr) =>
        // only one value, reduction trivial
        val reducedExprs = ExpressionReducer(expr).toList
        reducedExprs.map(e => call.copy(args = Seq(e)))

      case formals =>
        // there are n possible reductions since we
        // reduce only one of the rhss at a time
        formals.indices.toList.flatMap {
          i =>
            val expr = formals(i)
            val reduced = ExpressionReducer(expr).toSeq

            for {
              e <- reduced
            } yield {
              val reducedArgs = (formals.take(i) :+ e) ++ formals.drop(i + 1)
              call.copy(args = reducedArgs)
            }
        }
    }

    reductions match {
      case Nil        => None
      case reductions => Some(new ReduceCallExpr(call, reductions))
    }
  }
}