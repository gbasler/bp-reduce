package bpReduce
package reduction

import bpReduce.ast.Stmt.Call
import bpReduce.BaseSpecification

class ReduceCallExprTest extends BaseSpecification {

  "one reduction: exhaustive test" in {
    import ReductionChain._
    val origin: Call = "foo(a & b, c | d)"
    val reductions: Seq[Reduction] = Seq(
      // 1st round
      "foo(a & b, c | d)" -> "foo(a, c | d)",
      "foo(a & b, c | d)" -> "foo(b, c | d)",
      "foo(a & b, c | d)" -> "foo(F, c | d)",
      "foo(a & b, c | d)" -> "foo(a & b, c)",
      "foo(a & b, c | d)" -> "foo(a & b, d)",
      "foo(a & b, c | d)" -> "foo(a & b, T)",

      // 2nd round
      "foo(a, c | d)" -> "foo(a, c)",
      "foo(a, c | d)" -> "foo(a, d)",
      "foo(a, c | d)" -> "foo(a, T)",
      "foo(a, c | d)" -> "foo(T, c | d)",
      "foo(a, c | d)" -> "foo(F, c | d)",

      "foo(b, c | d)" -> "foo(b, c)",
      "foo(b, c | d)" -> "foo(b, d)",
      "foo(b, c | d)" -> "foo(b, T)",
      "foo(b, c | d)" -> "foo(T, c | d)",
      "foo(b, c | d)" -> "foo(F, c | d)",

      "foo(a & b, c)" -> "foo(a, c)",
      "foo(a & b, c)" -> "foo(b, c)",
      "foo(a & b, c)" -> "foo(F, c)",
      "foo(a & b, c)" -> "foo(a & b, T)",
      "foo(a & b, c)" -> "foo(a & b, F)",

      "foo(a & b, d)" -> "foo(a, d)",
      "foo(a & b, d)" -> "foo(b, d)",
      "foo(a & b, d)" -> "foo(F, d)",
      "foo(a & b, d)" -> "foo(a & b, T)",
      "foo(a & b, d)" -> "foo(a & b, F)",

      // 3rd round
      "foo(a, c)" -> "foo(a, T)",
      "foo(a, c)" -> "foo(a, F)",
      "foo(a, c)" -> "foo(T, c)",
      "foo(a, c)" -> "foo(F, c)",

      "foo(a, d)" -> "foo(a, T)",
      "foo(a, d)" -> "foo(a, F)",
      "foo(a, d)" -> "foo(T, d)",
      "foo(a, d)" -> "foo(F, d)",

      "foo(a, T)" -> "foo(T, T)",
      "foo(a, T)" -> "foo(F, T)",
      "foo(a, F)" -> "foo(T, F)",
      "foo(a, F)" -> "foo(F, F)",

      "foo(T, c | d)" -> "foo(T, c)",
      "foo(T, c | d)" -> "foo(T, d)",
      "foo(T, c | d)" -> "foo(T, T)",

      "foo(F, c | d)" -> "foo(F, c)",
      "foo(F, c | d)" -> "foo(F, d)",
      "foo(F, c | d)" -> "foo(F, T)",

      "foo(b, c)" -> "foo(b, T)",
      "foo(b, c)" -> "foo(b, F)",
      "foo(b, c)" -> "foo(T, c)",
      "foo(b, c)" -> "foo(F, c)",

      "foo(b, d)" -> "foo(b, T)",
      "foo(b, d)" -> "foo(b, F)",
      "foo(b, d)" -> "foo(T, d)",
      "foo(b, d)" -> "foo(F, d)",

      "foo(b, T)" -> "foo(T, T)",
      "foo(b, T)" -> "foo(F, T)",
      "foo(b, F)" -> "foo(T, F)",
      "foo(b, F)" -> "foo(F, F)",

      "foo(T, c)" -> "foo(T, T)",
      "foo(T, c)" -> "foo(T, F)",
      "foo(F, c)" -> "foo(F, T)",
      "foo(F, c)" -> "foo(F, F)",

      "foo(a & b, T)" -> "foo(a, T)",
      "foo(a & b, T)" -> "foo(b, T)",
      "foo(a & b, T)" -> "foo(F, T)",

      "foo(a & b, F)" -> "foo(a, F)",
      "foo(a & b, F)" -> "foo(b, F)",
      "foo(a & b, F)" -> "foo(F, F)",

      "foo(T, d)" -> "foo(T, T)",
      "foo(T, d)" -> "foo(T, F)",
      "foo(F, d)" -> "foo(F, T)",
      "foo(F, d)" -> "foo(F, F)",

      "foo(a & b, T)" -> "foo(a, T)",
      "foo(a & b, T)" -> "foo(b, T)",
      "foo(a & b, T)" -> "foo(F, T)",

      "foo(a & b, F)" -> "foo(a, F)",
      "foo(a & b, F)" -> "foo(b, F)",
      "foo(a & b, F)" -> "foo(F, F)"
    )

    val reducer = ReduceCallExpr(origin).get
    ReductionChecker(reducer, origin, reductions)
  }
}

