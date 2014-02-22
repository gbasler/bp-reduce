package bpReduce
package reduction

import bpReduce.ast.Stmt
import bpReduce.ast.Stmt._
import bpReduce.ast.Stmt.Assign

/**
 * Reduces `:=` expressions by incrementally omitting assignments
 * (we don't reduce until the `skip` statement, since this is the task of
 * the [[bpReduce.reduction.Reducers.ReplaceWithSkip]] reducer).
 *
 * @param next
 */
final case class ReduceAssign(from: Assign, next: List[Assign]) extends StmtReducer {
  require(next.nonEmpty)

  def to: Assign = next.head

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
      // no reduction possible, if only one assignment
      case assign: Assign if assign.assigns.size > 1 =>
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
      case _                                          =>
        None
    }
  }
}