package bpReduce
package transformations

import bpReduce.ast.{Expr, Program}
import bpReduce.ast.Stmt.Assign
import bpReduce.ast.Expr.Var

case class ReduceAssign2(assign: Assign) {

  val n = assign.assigns.size
  // there are 2^n -1 possible reductions...
  // however reducing all assigns would be equal to replace it with skip...
  // removing the assign has the disadvantage that if it was a jump target, the whole
  // program must be transformed

  //  assign.assigns.combinations()

  val reductions: Seq[Seq[(Var, Expr)]] = for {
    i <- assign.assigns.indices
  } yield {
    assign.assigns.take(i) ++ assign.assigns.drop(i + 1)
  }

  val assigns: Seq[Assign] = for {
    assigns <- reductions
  } yield {
    assign.copy(assigns = assigns)
  }

  val current = assigns.headOption

  def transform: Option[Assign] = {
    // return next assign...
    current
  }

  /**
   * @return New transformer with updated internal state for next transformation (if possible).
   */
  def advance(program: Program): Option[Transformer] = {
    // reduce current assign...
    ReduceAssign2(current)
    ???
  }
}
