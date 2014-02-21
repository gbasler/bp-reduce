package bpReduce
package transformations
package reduction

import bpReduce.ast.Stmt
import bpReduce.ast.Stmt._
import bpReduce.ast.Stmt.Assign

/**
 * Reduces `:=` expressions by incrementally omitting assignments
 * (until we arrive at the `skip` statement).
 *
 * @param next
 */
final case class ReduceAssign(from: Assign, next: List[Stmt]) extends StmtReducer {
  require(next.nonEmpty)

  // stmt because we could reduce to `Skip`
  def to: Stmt = next.head

  def reduce: Option[ReduceAssign] = ReduceAssign(to)

  def advance: Option[ReduceAssign] = {
    // previous reduction not successful: 
    // try next reduction 
    next match {
      case last :: Nil  => None
      case head :: tail => Some(copy(next = tail))
    }
  }
}

object ReduceAssign {
  def apply(stmt: Stmt): Option[ReduceAssign] = {

    stmt match {
      case assign: Assign =>
        if (assign.assigns.size < 2) {
          // only one assignment, reduction trivial
          Some(new ReduceAssign(assign, Skip :: Nil))
        } else {
          // there are 2^n -1 possible reductions...
          // however reducing all assigns would be equal to replace it with skip...
          // removing the assign has the disadvantage that if it was a jump target, the whole
          // program must be transformed

          val reductions = for {
            i <- assign.assigns.indices.reverse.toList
          } yield {
            val assigns = assign.assigns.take(i) ++ assign.assigns.drop(i + 1)
            assign.copy(assigns = assigns)
          }

          Some(new ReduceAssign(assign, reductions))
        }
      case _              =>
        None
    }
  }
}