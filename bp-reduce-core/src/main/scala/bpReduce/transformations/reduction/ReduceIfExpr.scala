package bpReduce
package transformations
package reduction

import bpReduce.ast.Stmt.If
import bpReduce.ast.Expr

// TODO: generalize?
final case class ReduceIfExpr(from: If,
                              reductions: List[Expr]) extends StmtReducer {

  require(reductions.nonEmpty)

  val to: If = from.copy(reductions.head)

  def reduce: Option[StmtReducer] = ReduceIfExpr(to, reductions.head)

  def advance: Option[ReduceIfExpr] = reductions match {
    case _ :: Nil     => None
    case head :: tail => Some(new ReduceIfExpr(from, tail))
  }
}

object ReduceIfExpr {
  def apply(iff: If): Option[ReduceIfExpr] = {
    apply(iff, iff.condition)
  }

  private def apply(iff: If, e: Expr) = {
    ExpressionReducer(e).toList match {
      case Nil        => None
      case reductions => Some(new ReduceIfExpr(iff, reductions))
    }
  }
}