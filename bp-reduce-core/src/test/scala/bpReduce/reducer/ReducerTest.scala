package bpReduce
package reducer

import bpReduce.ast.Program
import bpReduce.reduction.Reducers
import bpReduce.reader.BooleanProgramParser

class ReducerTest extends BaseSpecification {
  implicit def fromText(program: String) = {
    new BooleanProgramParser().parse(program)
  }

  "reducer algorithm" should {
    "one liner" in {
      val allAcceptChecker = new Checker {
        def apply(program: Program, iteration: Int): CheckerResult = CheckerResult.Accept
      }

      val config = ReducerConfig(List(Reducers.ReplaceWithSkip), allAcceptChecker, simplify = false)

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

      Reducer(config, verbose = false)(program) must beSameProgram(skip)
    }

    "two liner" in {
      val allAcceptChecker = new Checker {
        def apply(program: Program, iteration: Int): CheckerResult = CheckerResult.Accept
      }

      val config = ReducerConfig(List(Reducers.ReplaceWithSkip), allAcceptChecker, simplify = false)

      val program: Program =
        """|void main()
          |begin
          |return;
          |return;
          |end
          |
        """.stripMargin

      val skip: Program =
        """|void main()
          |begin
          |skip;
          |skip;
          |end
          |
        """.stripMargin

      Reducer(config, verbose = false)(program) must beSameProgram(skip)
    }

    "two liner: no reduction possible" in {
      val allRejectChecker = new Checker {
        def apply(program: Program, iteration: Int): CheckerResult = CheckerResult.Reject
      }

      val config = ReducerConfig(List(Reducers.ReplaceWithSkip), allRejectChecker, simplify = false)

      val program: Program =
        """|void main()
          |begin
          |return;
          |return;
          |end
          |
        """.stripMargin

      Reducer(config, verbose = false)(program) must beSameProgram(program)
    }
  }
}
