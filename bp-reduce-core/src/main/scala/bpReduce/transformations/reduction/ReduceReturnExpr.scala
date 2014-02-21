package bpReduce
package transformations
package reduction

import bpReduce.ast.Stmt
import bpReduce.ast.Stmt.{Return, Assign}

/**
 * Reduces `return e1, e2, ... en` expressions by incrementally reducing the expressions.
 *
 * The algorithm reduces only one part of the returned values per iteration.
 *
 * @param next The next possible reduction for [[StmtReducer.advance]].
 */
final case class ReduceReturnExpr(from: Stmt, next: List[Stmt]) extends StmtReducer {

  require(next.nonEmpty)

  def to: Stmt = next.head

  def reduce: Option[ReduceReturnExpr] = {
    ReduceReturnExpr(to)
  }

  def advance: Option[ReduceReturnExpr] = {
    // previous reduction not successful:
    // try next reduction
    next match {
      case _ :: Nil     => None
      case head :: tail => Some(copy(next = tail))
    }
  }
}

object ReduceReturnExpr {
  def apply(stmt: Stmt): Option[ReduceReturnExpr] = {

    stmt match {
      case assign: Assign => apply(assign)
      case _              => None
    }
  }

  def apply(ret: Return): Option[ReduceReturnExpr] = {

    val reductions = ret.values match {
      case Seq(expr) =>
        // only one value, reduction trivial
        val reducedExprs = ExpressionReducer(expr).toList
        reducedExprs.map(e => ret.copy(values = Seq(e)))

      case returns =>
        // there are n possible reductions since we
        // reduce only one of the rhss at a time
        returns.indices.toList.flatMap {
          i =>
            val expr = returns(i)
            val reduced = ExpressionReducer(expr).toSeq

            for {
              e <- reduced
            } yield {
              val reducedAssigns = (returns.take(i) :+ e) ++ returns.drop(i + 1)
              ret.copy(values = reducedAssigns)
            }
        }
    }

    reductions match {
      case Nil        => None
      case reductions => Some(new ReduceReturnExpr(ret, reductions))
    }
  }
}