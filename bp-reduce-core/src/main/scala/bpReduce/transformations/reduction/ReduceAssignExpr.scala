package bpReduce
package transformations
package reduction

import bpReduce.ast.{Expr, Stmt}
import bpReduce.ast.Stmt.Assign
import bpReduce.ast.Expr.Var

/**
 * Reduces `:=` expressions by incrementally reducing the expressions at the rhs and
 * the `constrain` expression.
 *
 * The algorithm reduces only one part of the assignment per iteration, either the constrain
 * or one of the rhss.
 *
 * @param next The next possible reduction for [[StmtReducer.advance]].
 */
final case class ReduceAssignExpr(next: List[Stmt]) extends StmtReducer {

  // stmt because we could reduce to `Skip`
  def current: Option[Stmt] = next.headOption

  def reduce: Option[ReduceAssignExpr] = {
    // last possible reduction is `Skip`,
    // so if next one is not a skip we can reduce more
    current collect {
      case assign: Assign => ReduceAssignExpr(assign)
    }
  }

  def advance: Option[ReduceAssignExpr] = {
    // previous reduction not successful: 
    // try next reduction 
    next match {
      case Nil          => None
      case head :: tail => Some(copy(next = tail))
    }
  }
}

object ReduceAssignExpr {
  def apply(stmt: Stmt): ReduceAssignExpr = {

    stmt match {
      case assign: Assign =>

        // reduce either constrain or expressions (one at a time)
        val reducedConstrains = assign.constrain.toList.flatMap {
          constrain =>
            val reducedConstrains = ExpressionReducer(constrain).toList
            reducedConstrains.map(c => assign.copy(constrain = Some(c)))
        }

        assign.assigns match {
          case Seq((variable, expr)) =>
            // only one assignment, reduction trivial
            val reducedExprs = ExpressionReducer(expr).toList
            val reducedRhs = reducedExprs.map(e => assign.copy(assigns = Seq(variable -> e)))
            new ReduceAssignExpr(reducedRhs ++ reducedConstrains)

          case assigns =>
            // there are n possible reductions since we
            // reduce only one of the rhss at a time
            val reductions = for {
              i <- assigns.indices.toList
            } yield {

              val reducedAssigns = for {
                (ve@(variable, expr), index) <- assigns.zipWithIndex
              } yield {
                if (index == i) {
                  val reduced = ExpressionReducer(expr).toSeq
                  val a = reduced.map(e => variable -> e)
                } else {
                  ve
                }
              }


              val reducedAssigns = (assigns.take(i) :+ (assigns(i)._1 -> e)) ++ assigns.drop(i + 1)
              assign.copy(assigns = reducedAssigns)
            }

            new ReduceAssignExpr(reductions)
        }
    }
  }
}