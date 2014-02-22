package bpReduce
package reduction

import bpReduce.ast.Stmt.Assume
import bpReduce.BaseSpecification

class ReduceAssumeTest extends BaseSpecification {

  "one reduction: exhaustive test" in {
    import ReductionChain._
    val origin: Assume = "assume(l = g)"
    val reductions: Seq[Reduction] = Seq(
      // 1st round
      "assume(l = g)" -> "assume(l)",
      "assume(l = g)" -> "assume(!l)",
      "assume(l = g)" -> "assume(g)",
      "assume(l = g)" -> "assume(!g)",

      // 2nd round
      "assume(l)" -> "assume(T)",
      "assume(l)" -> "assume(F)",
      "assume(!l)" -> "assume(T)",
      "assume(!l)" -> "assume(F)", 
      "assume(g)" -> "assume(T)",
      "assume(g)" -> "assume(F)",
      "assume(!g)" -> "assume(T)",
      "assume(!g)" -> "assume(F)"
    )

    val reducer = ReduceAssumeExpr(origin).get
    ReductionChecker(reducer, origin, reductions)
  }
}

