package bpReduce
package reducer

import bpReduce.ast.Program
import bpReduce.transformations.reduction.Reducers
import bpReduce.reader.BooleanProgramParser

class ReducerTest extends BaseSpecification {
  implicit def fromText(program: String) = {
    new BooleanProgramParser().parse(program)
  }

  "reducer algorithm" should {
    "one liner" in {
      val allAcceptChecker = new Checker {
        override def apply(program: Program): CheckerResult = CheckerResult.Accept
      }

      val config = ReducerConfig(List(Reducers.ReplaceWithSkip), allAcceptChecker)

      val program: Program =
        """|void main()
          |begin
          |return;
          |end
          |
        """.stripMargin

      val skip: Program =
        """|void main()
          |begin
          |skip;
          |end
          |
        """.stripMargin

      Reducer(config)(program) must beSameProgram(skip)
    }
  }
}
