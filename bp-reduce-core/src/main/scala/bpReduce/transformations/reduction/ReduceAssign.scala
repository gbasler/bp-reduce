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
final case class ReduceAssign(next: List[Stmt]) extends StmtReducer {

  // stmt because we could reduce to `Skip`
  def current: Option[Stmt] = next.headOption

  def reduce: Option[ReduceAssign] = {
    // last possible reduction is `Skip`,
    // so if next one is not a skip we can reduce more
    current collect {
      case assign: Assign => ReduceAssign(assign)
    }
  }

  def advance: Option[ReduceAssign] = {
    // previous reduction not successful: 
    // try next reduction 
    next match {
      case Nil          => None
      case head :: tail => Some(copy(next = tail))
    }
  }
}

object ReduceAssign {
  def apply(stmt: Stmt): ReduceAssign = {

    stmt match {
      case assign: Assign =>
        if (assign.assigns.size < 2) {
          // only one assignment, reduction trivial
          new ReduceAssign(Skip :: Nil)
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

          new ReduceAssign(reductions)
        }
      case _              =>
        new ReduceAssign(Nil)
    }
  }
}