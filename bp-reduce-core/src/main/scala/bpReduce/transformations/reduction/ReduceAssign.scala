package bpReduce
package transformations
package reduction

import bpReduce.ast.Stmt
import bpReduce.ast.Stmt._
import bpReduce.ast.Stmt.Assign
import scala.Some

case class ReduceAssign(assign: Assign, next: List[Stmt]) extends StmtReducer {

//  override def isDefinedAt(s: Stmt) = s match {
//    case _: Assign => true
//    case _         => false
//  }

  // stmt because we could reduce to `Skip`
  def current: Option[Stmt] = next.headOption

  def reduce: Option[ReduceAssign] = {
    // reduction successful, reduce more if possible
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
  def apply(assign: Assign): ReduceAssign = {
    // there are 2^n -1 possible reductions...
    // however reducing all assigns would be equal to replace it with skip...
    // removing the assign has the disadvantage that if it was a jump target, the whole
    // program must be transformed

    if (assign.assigns.size < 2) {
      new ReduceAssign(assign, Skip :: Nil)
    } else {

      val reductions = for {
        i <- assign.assigns.indices.reverse.toList
      } yield {
        val assigns = assign.assigns.take(i) ++ assign.assigns.drop(i + 1)
        assign.copy(assigns = assigns)
      }

      new ReduceAssign(assign, reductions)
    }
  }
}