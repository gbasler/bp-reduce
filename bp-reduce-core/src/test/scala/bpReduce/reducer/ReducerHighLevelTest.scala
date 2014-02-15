package bpReduce
package reducer

import bpReduce.ast.{LabelledStmt, Program}
import bpReduce.transformations.reduction.Reducers
import bpReduce.reader.BooleanProgramParser

class ReducerHighLevelTest extends BaseSpecification {
  implicit def fromText(program: String) = {
    new BooleanProgramParser().parse(program)
  }

  "complex reducer algorithm" should {
    "reduce with smart checker" in {

      val smartChecker = new Checker {
        override def apply(program: Program): CheckerResult = {
          val ok = program.functions.exists(_.stmts.exists {
            stmt: LabelledStmt => true
          })
          if (ok) {
            CheckerResult.Accept
          } else {
            CheckerResult.Reject
          }

        }
      }

      val config = ReducerConfig(Reducers.All, smartChecker)

      val program: Program =
        """|void main()
          |begin
          |return;
          |return;
          |end
          |
        """.stripMargin

      Reducer(config)(program) must beSameProgram(program)
    }
  }
}
