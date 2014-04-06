package bpReduce
package reduction

import bpReduce.ast.Stmt.Assume
import bpReduce.ast.Expr

// TODO: generalize?
final case class ReduceAssumeExpr(from: Assume,
                                  reductions: List[Expr]) extends StmtReducer {

  require(reductions.nonEmpty)

  val to: Assume = from.copy(reductions.head)

  def reduce: Option[StmtReducer] = ReduceAssumeExpr(to, reductions.head)

  def advance: Option[ReduceAssumeExpr] = reductions match {
    case _ :: Nil     => None
    case head :: tail => Some(new ReduceAssumeExpr(from, tail))
  }
}

object ReduceAssumeExpr {
  def apply(assume: Assume): Option[ReduceAssumeExpr] = {
    apply(assume, assume.e)
  }

  private def apply(assume: Assume, e: Expr) = {
    ExpressionReducer(e).toList match {
      case Nil        => None
      case reductions => Some(new ReduceAssumeExpr(assume, reductions))
    }
  }
}