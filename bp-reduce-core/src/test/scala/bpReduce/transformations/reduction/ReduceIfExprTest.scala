package bpReduce
package transformations
package reduction

import bpReduce.ast.Stmt.If
import bpReduce.BaseSpecification

class ReduceIfExprTest extends BaseSpecification {

  "one reduction: exhaustive test" in {
    import ReductionChain._
    val origin: If = "if * then goto l6; fi"
    val reductions: Seq[Reduction] = Seq(
      // 1st round
      "if * then goto l6; fi" -> "if T then goto l6; fi",
      "if * then goto l6; fi" -> "if F then goto l6; fi"
    )

    val reducer = ReduceIfExpr(origin).get
    ReductionChecker(reducer, origin, reductions)
  }
}

