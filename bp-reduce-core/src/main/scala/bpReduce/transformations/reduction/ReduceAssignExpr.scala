package bpReduce
package transformations
package reduction

import bpReduce.ast.{Expr, Stmt}
import bpReduce.ast.Stmt.Assign

/**
 * Reduces `:=` expressions by incrementally reducing the expressions at the rhs and
 * the `constrain` expression.
 *
 * @param next
 */
final case class ReduceAssignExpr(next: List[Expr => Stmt]) extends StmtReducer {

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

    def reduceConstrain(e: Expr) = {

    }

    stmt match {
      case assign: Assign =>
        assign.assigns match {
          case Seq((variable, expr)) =>
            // only one assignment, reduction trivial
            def reducer(e: Expr) = {
              assign.copy(assigns = Seq(variable -> e))
            }
            new ReduceAssignExpr(reducer _ :: Nil)

          case assigns =>
            // there are 2^n -1 possible reductions...
            // however reducing all assigns would be equal to replace it with skip...
            // removing the assign has the disadvantage that if it was a jump target, the whole
            // program must be transformed
            val reductions = for {
              i <- assign.assigns.indices.reverse.toList
            } yield {
              e: Expr =>
                val reducedAssigns = (assigns.take(i) :+ (assigns(i)._1 -> e)) ++ assigns.drop(i + 1)
                assign.copy(assigns = reducedAssigns)
            }

            new ReduceAssignExpr(reductions)
        }
    }
  }
}