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

    val origin: Assign = "l0 := * constrain(l0 = l1)"

    // 1st round
    val round1Stmt1: Assign = "l0 := T constrain(l0 = l1)"
    val round1Stmt2: Assign = "l0 := F constrain(l0 = l1)"

    val round1Stmt3: Assign = "l0 := * constrain(l0)"
    val round1Stmt4: Assign = "l0 := * constrain(!l0)"
    val round1Stmt5: Assign = "l0 := * constrain(l1)"
    val round1Stmt6: Assign = "l0 := * constrain(!l1)"

    // 2nd round
    val round2Stmt1: Assign = "l0 := T constrain(l0)"
    val round2Stmt2: Assign = "l0 := T constrain(!l0)"
    val round2Stmt3: Assign = "l0 := T constrain(l1)"
    val round2Stmt4: Assign = "l0 := T constrain(!l1)"

    val round2Stmt5: Assign = "l0 := F constrain(l0)"
    val round2Stmt6: Assign = "l0 := F constrain(!l0)"
    val round2Stmt7: Assign = "l0 := F constrain(l1)"
    val round2Stmt8: Assign = "l0 := F constrain(!l1)"

    val round2Stmt9: Assign = "l0 := * constrain(T)"
    val round2Stmt10: Assign = "l0 := * constrain(F)"

    // 3rd round
    val round3Stmt1: Assign = "l0 := T constrain(T)"
    val round3Stmt2: Assign = "l0 := T constrain(F)"

    val round3Stmt3: Assign = "l0 := F constrain(T)"
    val round3Stmt4: Assign = "l0 := F constrain(F)"

    import ReductionChain._
    val reductions: Seq[Reduction] = Seq(
      // 1st round
      origin reducesTo round1Stmt1,
      origin reducesTo round1Stmt2,
      origin reducesTo round1Stmt3,
      origin reducesTo round1Stmt4,
      origin reducesTo round1Stmt5,
      origin reducesTo round1Stmt6,

      // 2nd round
      round1Stmt1 reducesTo round2Stmt1,
      round1Stmt1 reducesTo round2Stmt2,
      round1Stmt1 reducesTo round2Stmt3,
      round1Stmt1 reducesTo round2Stmt4,
      round1Stmt2 reducesTo round2Stmt5,
      round1Stmt2 reducesTo round2Stmt6,
      round1Stmt2 reducesTo round2Stmt7,
      round1Stmt2 reducesTo round2Stmt8,
      round1Stmt3 reducesTo round2Stmt9,
      round1Stmt3 reducesTo round2Stmt10,
      round1Stmt4 reducesTo round2Stmt9,
      round1Stmt4 reducesTo round2Stmt10,
      round1Stmt5 reducesTo round2Stmt9,
      round1Stmt5 reducesTo round2Stmt10,
      round1Stmt6 reducesTo round2Stmt9,
      round1Stmt6 reducesTo round2Stmt10,

      // 3rd round
      round2Stmt1 reducesTo round3Stmt1,
      round2Stmt1 reducesTo round3Stmt2,
      round2Stmt2 reducesTo round3Stmt1,
      round2Stmt2 reducesTo round3Stmt2,
      round2Stmt3 reducesTo round3Stmt1,
      round2Stmt3 reducesTo round3Stmt2,
      round2Stmt4 reducesTo round3Stmt1,
      round2Stmt4 reducesTo round3Stmt2,

      round2Stmt5 reducesTo round3Stmt3,
      round2Stmt5 reducesTo round3Stmt4,
      round2Stmt6 reducesTo round3Stmt3,
      round2Stmt6 reducesTo round3Stmt4,
      round2Stmt7 reducesTo round3Stmt3,
      round2Stmt7 reducesTo round3Stmt4,
      round2Stmt8 reducesTo round3Stmt3,
      round2Stmt8 reducesTo round3Stmt4
    )

    val tree = buildTree(reductions)

    // checks reduction from the point of view from the reducer
    def checkReductionChain(root: Stmt, reducer: StmtReducer) {

      def checkReferenceReduction(from: Stmt, to: Stmt) = {
        if(!tree.contains(from)) {
          println("asdasd")
        }
        val refTos = tree.getOrElse(from, sys.error(s"there should be a reduction possible from $root to $to"))
        if(!refTos.contains(to)) {
          println("asdasd")
          val r = reducer.reduce // redo
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

    val reducer: ReduceAssignExpr = ReduceAssignExpr(origin).get

    checkReductionChain(origin, reducer)

    reducer.current.get === round1Stmt1
    reducer.reduce.get.current.get === round2Stmt1
    reducer.reduce.get.reduce must beNone
    reducer.advance.get.current.get === round1Stmt2
    reducer.advance.get.reduce.get.current.get === round2Stmt2

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