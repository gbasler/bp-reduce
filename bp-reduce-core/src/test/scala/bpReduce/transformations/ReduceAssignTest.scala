package bpReduce
package transformations

import bpReduce.reader.BooleanProgramParser
import bpReduce.ast.{Stmt, LabelledStmt, Function, Program}
import bpReduce.ast.Stmt.Skip

class ReduceAssignTest extends BaseSpecification {
  "one reduction" in {
    implicit def fromString(str: String): Stmt = {
      new BooleanProgramParser().parseStmt(str)
    }

    val stmt: Stmt = "l0, l1 := l1, l0"
    val stmt1: Stmt = "l0 := l1"
    val stmt2: Stmt = "l1 := l0"

    val reducer: ReduceAssign2 = new ReduceAssign2(stmt)
    reducer.transform() must beSome(stmt1)
    reducer.advance() must beSome(stmt2)


    ok
  }
}
