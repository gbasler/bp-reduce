package bpReduce
package transformations
package reduction

import bpReduce.BaseSpecification
import bpReduce.ast.Stmt.Return

class ReduceReturnExprTest extends BaseSpecification {

  "exhaustive reduction / advance test" in {
    import ReductionChain._
    val origin: Return = "return l1, l0"
    val reductions: Seq[Reduction] = Seq(
      // 1st round
      "return l1, l0" -> "return T, l0",
      "return l1, l0" -> "return F, l0",
      "return l1, l0" -> "return l1, T",
      "return l1, l0" -> "return l1, F",

      // 2nd round
      "return T, l0" -> "return T, T",
      "return T, l0" -> "return T, F",
      "return F, l0" -> "return F, T",
      "return F, l0" -> "return F, F",

      "return l1, T" -> "return T, T",
      "return l1, T" -> "return F, T",
      "return l1, F" -> "return T, F",
      "return l1, F" -> "return F, F"
    )

    val reducer = ReduceReturnExpr(origin).get
    ReductionChecker(reducer, origin, reductions)
  }
}

