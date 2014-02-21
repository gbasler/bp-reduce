package bpReduce
package transformations
package reduction

import bpReduce.ast.Stmt
import bpReduce.ast.Stmt.Assign

/**
 * Reduces `:=` expressions by incrementally reducing the expressions at the rhs and
 * the `constrain` expression.
 *
 * The algorithm reduces only one part of the assignment per iteration, either the constrain
 * or one of the rhss.
 *
 * @param next The next possible reduction for [[StmtReducer.advance]].
 */
final case class ReduceAssignExpr(from: Stmt, next: List[Stmt]) extends StmtReducer {

  require(next.nonEmpty)

  def to: Stmt = next.head

  def reduce: Option[ReduceAssignExpr] = {
    ReduceAssignExpr(to)
  }

  def advance: Option[ReduceAssignExpr] = {
    // previous reduction not successful: 
    // try next reduction 
    next match {
      case _ :: Nil     => None
      case head :: tail => Some(copy(next = tail))
    }
  }
}

object ReduceAssignExpr {
  def apply(stmt: Stmt): Option[ReduceAssignExpr] = {

    stmt match {
      case assign: Assign => apply(assign)
      case _              => None
    }
  }

  def apply(assign: Assign): Option[ReduceAssignExpr] = {

    // reduce either constrain or expressions (one at a time)
    val reducedConstrains = assign.constrain.toList.flatMap {
      constrain =>
        val reducedConstrains = ExpressionReducer(constrain).toList
        reducedConstrains.map(c => assign.copy(constrain = Some(c)))
    }

    val reducesRhss = assign.assigns match {
      case Seq((variable, expr)) =>
        // only one assignment, reduction trivial
        val reducedExprs = ExpressionReducer(expr).toList
        reducedExprs.map(e => assign.copy(assigns = Seq(variable -> e)))

      case assigns =>
        // there are n possible reductions since we
        // reduce only one of the rhss at a time
        assigns.indices.toList.flatMap {
          i =>
            val (variable, expr) = assigns(i)
            val reduced = ExpressionReducer(expr).toSeq

            for {
              e <- reduced
            } yield {
              val reducedAssigns = (assigns.take(i) :+ (variable -> e)) ++ assigns.drop(i + 1)
              assign.copy(assigns = reducedAssigns)
            }
        }
    }

    val reductions = reducesRhss ++ reducedConstrains
    reductions match {
      case Nil        => None
      case reductions => Some(new ReduceAssignExpr(assign, reductions))
    }
  }
}