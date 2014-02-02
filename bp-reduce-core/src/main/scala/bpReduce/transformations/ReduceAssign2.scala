package bpReduce
package transformations

import bpReduce.ast.Stmt
import bpReduce.ast.Stmt.{Skip, Assign}

case class ReduceAssign2(assign: Assign, next: List[Stmt]) {

  // stmt because we could reduce to `Skip`
  def current: Option[Stmt] = next.headOption

  def reduce: Option[ReduceAssign2] = {
    // reduction successful, reduce more if possible
    current collect {
      case assign: Assign => ReduceAssignFactory(assign)
    }
  }

  def advance: Option[ReduceAssign2] = {
    // previous reduction not successful: 
    // try next reduction 
    next match {
      case Nil          => None
      case head :: tail => Some(copy(next = tail))
    }
  }
}

object ReduceAssignFactory {
  def apply(assign: Assign): ReduceAssign2 = {
    // there are 2^n -1 possible reductions...
    // however reducing all assigns would be equal to replace it with skip...
    // removing the assign has the disadvantage that if it was a jump target, the whole
    // program must be transformed

    if (assign.assigns.size < 2) {
      new ReduceAssign2(assign, Skip :: Nil)
    } else {

      val reductions = for {
        i <- assign.assigns.indices.toList
      } yield {
        val assigns = assign.assigns.take(i) ++ assign.assigns.drop(i + 1)
        assign.copy(assigns = assigns)
      }

      new ReduceAssign2(assign, reductions)
    }
  }
}