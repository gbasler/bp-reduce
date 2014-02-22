package bpReduce
package transformations
package reduction

import bpReduce.ast.Stmt.Assign
import bpReduce.BaseSpecification

class ReduceAssignTest extends BaseSpecification {
  "one reduction" in {
    import ReductionChain._
    val origin: Assign = "l0, l1 := l1, l0"
    val reductions: Seq[Reduction] = Seq(
      // 1st round
      "l0, l1 := l1, l0" -> "l0 := l1",
      "l0, l1 := l1, l0" -> "l1 := l0"
    )

    val reducer = ReduceAssign(origin).get
    ReductionChecker(reducer, origin, reductions)
  }
}
