package bpReduce
package reduction

import bpReduce.ast.Stmt.Assign
import bpReduce.BaseSpecification

class ReduceAssignExprTest extends BaseSpecification {

  "one reduction: exhaustive test" in {
    import ReductionChain._
    val origin: Assign = "l0, l1 := l1, l0"
    val reductions: Seq[Reduction] = Seq(
      // 1st round
      "l0, l1 := l1, l0" -> "l0, l1 := T, l0",
      "l0, l1 := l1, l0" -> "l0, l1 := F, l0",
      "l0, l1 := l1, l0" -> "l0, l1 := l1, T",
      "l0, l1 := l1, l0" -> "l0, l1 := l1, F",

      // 2nd round
      "l0, l1 := T, l0" -> "l0, l1 := T, T",
      "l0, l1 := T, l0" -> "l0, l1 := T, F",
      "l0, l1 := F, l0" -> "l0, l1 := F, T",
      "l0, l1 := F, l0" -> "l0, l1 := F, F",

      "l0, l1 := l1, T" -> "l0, l1 := T, T",
      "l0, l1 := l1, T" -> "l0, l1 := F, T",
      "l0, l1 := l1, F" -> "l0, l1 := T, F",
      "l0, l1 := l1, F" -> "l0, l1 := F, F"
    )

    val reducer = ReduceAssignExpr(origin).get
    ReductionChecker(reducer, origin, reductions)
  }

  "one reduction + constrain" in {
    import ReductionChain._
    val origin: Assign = "l0 := * constrain(l0 = l1)"
    val reductions: Seq[Reduction] = Seq(
      // 1st round
      "l0 := * constrain(l0 = l1)" -> "l0 := T constrain(l0 = l1)",
      "l0 := * constrain(l0 = l1)" -> "l0 := F constrain(l0 = l1)",
      "l0 := * constrain(l0 = l1)" -> "l0 := * constrain(l0)",
      "l0 := * constrain(l0 = l1)" -> "l0 := * constrain(!l0)",
      "l0 := * constrain(l0 = l1)" -> "l0 := * constrain(l1)",
      "l0 := * constrain(l0 = l1)" -> "l0 := * constrain(!l1)",

      // 2nd round
      "l0 := T constrain(l0 = l1)" -> "l0 := T constrain(l0)",
      "l0 := T constrain(l0 = l1)" -> "l0 := T constrain(!l0)",
      "l0 := T constrain(l0 = l1)" -> "l0 := T constrain(l1)",
      "l0 := T constrain(l0 = l1)" -> "l0 := T constrain(!l1)",
      "l0 := F constrain(l0 = l1)" -> "l0 := F constrain(l0)",
      "l0 := F constrain(l0 = l1)" -> "l0 := F constrain(!l0)",
      "l0 := F constrain(l0 = l1)" -> "l0 := F constrain(l1)",
      "l0 := F constrain(l0 = l1)" -> "l0 := F constrain(!l1)",
      "l0 := * constrain(l0)" -> "l0 := T constrain(l0)",
      "l0 := * constrain(l0)" -> "l0 := F constrain(l0)",
      "l0 := * constrain(l0)" -> "l0 := * constrain(T)",
      "l0 := * constrain(l0)" -> "l0 := * constrain(F)",
      "l0 := * constrain(!l0)" -> "l0 := T constrain(!l0)",
      "l0 := * constrain(!l0)" -> "l0 := F constrain(!l0)",
      "l0 := * constrain(!l0)" -> "l0 := * constrain(T)",
      "l0 := * constrain(!l0)" -> "l0 := * constrain(F)",
      "l0 := * constrain(l1)" -> "l0 := T constrain(l1)",
      "l0 := * constrain(l1)" -> "l0 := F constrain(l1)",
      "l0 := * constrain(l1)" -> "l0 := * constrain(T)",
      "l0 := * constrain(l1)" -> "l0 := * constrain(F)",
      "l0 := * constrain(!l1)" -> "l0 := T constrain(!l1)",
      "l0 := * constrain(!l1)" -> "l0 := F constrain(!l1)",
      "l0 := * constrain(!l1)" -> "l0 := * constrain(T)",
      "l0 := * constrain(!l1)" -> "l0 := * constrain(F)",

      // 3rd round
      "l0 := T constrain(l0)" -> "l0 := T constrain(T)",
      "l0 := T constrain(l0)" -> "l0 := T constrain(F)",
      "l0 := T constrain(!l0)" -> "l0 := T constrain(T)",
      "l0 := T constrain(!l0)" -> "l0 := T constrain(F)",
      "l0 := T constrain(l1)" -> "l0 := T constrain(T)",
      "l0 := T constrain(l1)" -> "l0 := T constrain(F)",
      "l0 := T constrain(!l1)" -> "l0 := T constrain(T)",
      "l0 := T constrain(!l1)" -> "l0 := T constrain(F)",

      "l0 := F constrain(l0)" -> "l0 := F constrain(T)",
      "l0 := F constrain(l0)" -> "l0 := F constrain(F)",
      "l0 := F constrain(!l0)" -> "l0 := F constrain(T)",
      "l0 := F constrain(!l0)" -> "l0 := F constrain(F)",
      "l0 := F constrain(l1)" -> "l0 := F constrain(T)",
      "l0 := F constrain(l1)" -> "l0 := F constrain(F)",
      "l0 := F constrain(!l1)" -> "l0 := F constrain(T)",
      "l0 := F constrain(!l1)" -> "l0 := F constrain(F)",

      "l0 := * constrain(T)" -> "l0 := T constrain(T)",
      "l0 := * constrain(T)" -> "l0 := F constrain(T)",
      "l0 := * constrain(F)" -> "l0 := T constrain(F)",
      "l0 := * constrain(F)" -> "l0 := F constrain(F)"
    )

    val reducer = ReduceAssignExpr(origin).get
    ReductionChecker(reducer, origin, reductions)
  }
}

