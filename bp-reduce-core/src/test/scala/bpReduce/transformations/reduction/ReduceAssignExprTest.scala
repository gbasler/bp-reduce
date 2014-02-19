package bpReduce
package transformations
package reduction

import bpReduce.reader.BooleanProgramParser
import bpReduce.ast.Stmt
import bpReduce.ast.Stmt.Assign
import bpReduce.BaseSpecification
import scala.annotation.tailrec

class ReduceAssignExprTest extends BaseSpecification {

  "one reduction: exhaustive test" in {
    implicit def stmtFromString(str: String): Stmt = {
      new BooleanProgramParser().parseStmt(str)
    }
    implicit def assignFromString(str: String): Assign = {
      new BooleanProgramParser().parseAssign(str)
    }

    val stmt: Assign = "l0, l1 := l1, l0"

    val stmt1: Assign = "l0, l1 := T, l0"
    val stmt2: Assign = "l0, l1 := T, T"

    val stmt3: Assign = "l0, l1 := F, l0"
    val stmt4: Assign = "l0, l1 := F, T"

    val stmt5: Assign = "l0, l1 := l1, T"
    val stmt6: Assign = "l0, l1 := l1, F"
    val stmt7: Assign = "l0, l1 := T, F"
    val stmt8: Assign = "l0, l1 := F, F"

    val reducer = ReduceAssignExpr(stmt).get

    // TODO: the order of advance shouldn't matter...
    // this test should be insensitive to it...

    reducer.current.get === stmt1
    reducer.reduce.get.current.get === stmt2
    reducer.reduce.get.reduce must beNone

    reducer.advance.get.current.get === stmt3
    reducer.advance.get.reduce.get.current.get === stmt4

    reducer.advance.get.advance.get.current.get === stmt5
    reducer.advance.get.advance.get.reduce.get.current.get === stmt2
    reducer.advance.get.advance.get.advance.get.current.get === stmt6
    reducer.advance.get.advance.get.advance.get.reduce.get.current.get === stmt7
    reducer.advance.get.advance.get.advance.get.reduce.get.current.get === stmt7
    reducer.advance.get.advance.get.advance.get.reduce.get.reduce must beNone
    reducer.advance.get.advance.get.advance.get.reduce.get.advance.get.current.get === stmt8
    reducer.advance.get.advance.get.advance.get.reduce.get.advance.get.reduce must beNone
    reducer.advance.get.advance.get.advance.get.reduce.get.advance.get.advance must beNone
  }

  "one reduction + constrain" in {
    implicit def stmtFromString(str: String): Stmt = {
      new BooleanProgramParser().parseStmt(str)
    }
    implicit def assignFromString(str: String): Assign = {
      new BooleanProgramParser().parseAssign(str)
    }

    val origin: Assign = "l0 := * constrain(l0 = l1)"
    import ReductionChain._
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
      "l0 := F constrain(!l1)" -> "l0 := F constrain(F)"
    )

    // we build a tree for convenience
    val tree = buildTree(reductions)

    // checks reduction from the point of view from the reducer
    @tailrec
    def checkReductionChain(root: Stmt,
                            reducer: StmtReducer,
                            unused: Set[Reduction]): Set[Reduction] = {

      def checkReferenceReduction(from: Stmt,
                                  to: Stmt,
                                  unused: Set[Reduction]) = {
        val refTos = tree.getOrElse(from, sys.error(s"there should be a reduction possible from $root to $to"))
        refTos must contain(to)
        unused - Reduction(from, to)
      }

      // see what reducer offers...
      val unusedWithoutCurrent = reducer.current match {
        case Some(to) =>
          checkReferenceReduction(root, to, unused)
        case None     =>
          // no reduction from this statement should be possible...
          tree.get(root) must beNone
          unused
      }

      val unusedWithoutReduced = reducer.reduce match {
        case Some(to) =>
          val from = reducer.current.getOrElse(sys.error(s"reduction without origin impossible"))
          checkReferenceReduction(from, to.current.get, unusedWithoutCurrent) // TODO: get seems weird...
        case None     =>
          unusedWithoutCurrent
      }

      reducer.advance match {
        case Some(x) =>
          // reduction possible... check
          checkReductionChain(root, x, unusedWithoutReduced) // TODO: get seems weird...
        case None    =>
          unusedWithoutReduced
      }
    }

    val reducer = ReduceAssignExpr(origin).get
    val leftOver = checkReductionChain(origin, reducer, reductions.toSet)
    leftOver must beEmpty
  }
}

object ReductionChain {

  implicit class UnparsedStmtWrapper(val stmt: String) extends AnyVal {
    def ->(s: String): Reduction = {
      val parser = new BooleanProgramParser()
      Reduction(parser.parseStmt(stmt), parser.parseStmt(s))
    }
  }

  implicit class StmtWrapper(val stmt: Stmt) extends AnyVal {
    def reducesTo(s: Stmt): Reduction = Reduction(stmt, s)
  }

  final case class Reduction(from: Stmt, to: Stmt)

  def buildTree(reductions: Seq[Reduction]): Map[Stmt, Set[Stmt]] = {
    reductions.groupBy(_.from).map {
      case (stmt, reductions) =>
        stmt -> reductions.map(_.to).toSet
    }
  }

}