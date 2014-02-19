package bpReduce
package transformations
package reduction

import bpReduce.reader.BooleanProgramParser
import bpReduce.ast.Stmt
import bpReduce.ast.Stmt.{Assign, Skip}
import bpReduce.{reducer, BaseSpecification}

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

    val stmt: Assign = "l0 := * constrain(l0 = l1)"

    // 1st round
    val stmt1: Assign = "l0 := T constrain(l0 = l1)"
    val stmt2: Assign = "l0 := F constrain(l0 = l1)"

    val stmt3: Assign = "l0 := * constrain(T)"
    val stmt4: Assign = "l0 := * constrain(F)"

    // 2nd round
    val stmt5: Assign = "l0 := T constrain(l0)"
    val stmt6: Assign = "l0 := T constrain(!l0)"
    val stmt7: Assign = "l0 := T constrain(l1)"
    val stmt8: Assign = "l0 := T constrain(!l1)"

    val stmt9: Assign = "l0 := F constrain(l0)"
    val stmt10: Assign = "l0 := F constrain(!l0)"
    val stmt11: Assign = "l0 := F constrain(l1)"
    val stmt12: Assign = "l0 := F constrain(!l1)"

    // 3rd round
    val stmt13: Assign = "l0 := T constrain(T)"
    val stmt14: Assign = "l0 := T constrain(F)"

    val stmt15: Assign = "l0 := F constrain(T)"
    val stmt16: Assign = "l0 := F constrain(F)"

    import ReductionChain._
    val reductions: Seq[Reduction] = Seq(
      // 1st round
      stmt reducesTo stmt1,
      stmt reducesTo stmt2,
      stmt reducesTo stmt3,
      stmt reducesTo stmt4,
      // 2nd round
      stmt1 reducesTo stmt5,
      stmt1 reducesTo stmt6,
      stmt1 reducesTo stmt7,
      stmt1 reducesTo stmt8,
      stmt2 reducesTo stmt9,
      stmt2 reducesTo stmt10,
      stmt2 reducesTo stmt11,
      stmt2 reducesTo stmt12,
      stmt3 reducesTo stmt13,
      stmt3 reducesTo stmt15,
      stmt4 reducesTo stmt14,
      stmt4 reducesTo stmt16,
      // 3rd round
      stmt5 reducesTo stmt13,
      stmt5 reducesTo stmt15,
      stmt6 reducesTo stmt13,
      stmt6 reducesTo stmt15,
      stmt7 reducesTo stmt13,
      stmt7 reducesTo stmt15,
      stmt8 reducesTo stmt13,
      stmt8 reducesTo stmt15,

      stmt9 reducesTo stmt13,
      stmt9 reducesTo stmt15,
      stmt10 reducesTo stmt13,
      stmt10 reducesTo stmt15,
      stmt11 reducesTo stmt13,
      stmt11 reducesTo stmt15,
      stmt12 reducesTo stmt13,
      stmt12 reducesTo stmt15
    )

    val tree = buildTree(reductions)

    // checks reduction from the point of view from the reducer
    def checkReductionChain(root: Stmt, reducer: StmtReducer) {

      def checkReferenceReduction(from: Stmt, to: Stmt) = {
        val refTos = tree.getOrElse(from, sys.error(s"there should be a reduction possible from $root to $to"))
        if(!refTos.contains(to)) {
          println("asdasd")
        }
        refTos must contain(to)
      }

      // see what reducer offers...
      reducer.current match {
        case Some(to) =>
          checkReferenceReduction(root, to)
        case None     =>
          // no reduction from this statement should be possible...
          tree.get(root) must beNone
      }

      reducer.reduce match {
        case Some(to) =>
          val from = reducer.current.getOrElse(sys.error(s"reduction without origin impossible"))
          checkReferenceReduction(from, to.current.get) // TODO: get seems weird...
        case None     =>
      }

      reducer.advance match {
        case Some(x) =>
          // reduction possible... check
          checkReductionChain(root, x) // TODO: get seems weird...
        case None    =>
      }
    }

    // TODO: check reduction from point of view of the reductioh chain...

    val reducer: ReduceAssignExpr = ReduceAssignExpr(stmt).get

    checkReductionChain(stmt, reducer)

    reducer.current.get === stmt1
    reducer.reduce.get.current.get === stmt5
    reducer.reduce.get.reduce must beNone
    reducer.advance.get.current.get === stmt2
    reducer.advance.get.reduce.get.current.get === stmt6

  }
}

object ReductionChain {

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