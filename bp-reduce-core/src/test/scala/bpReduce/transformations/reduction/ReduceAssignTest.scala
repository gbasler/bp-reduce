package bpReduce
package transformations
package reduction

import bpReduce.reader.BooleanProgramParser
import bpReduce.ast.Stmt
import bpReduce.ast.Stmt.{Assign, Skip}
import bpReduce.BaseSpecification

class ReduceAssignTest extends BaseSpecification {
  "one reduction" in {
    implicit def stmtFromString(str: String): Stmt = {
      new BooleanProgramParser().parseStmt(str)
    }
    implicit def assignFromString(str: String): Assign = {
      new BooleanProgramParser().parseAssign(str)
    }

    val stmt: Assign = "l0, l1 := l1, l0"
    val stmt1: Assign = "l0 := l1"
    val stmt2: Assign = "l1 := l0"

    val reducer: ReduceAssign = ReduceAssign(stmt).get
    reducer.current must beSome(stmt1)
    reducer.reduce.get.current must beSome(Skip)
    reducer.reduce.get.reduce must beNone
    reducer.advance.get.current must beSome(stmt2)
    reducer.advance.get.reduce.get.current must beSome(Skip)
  }
}
