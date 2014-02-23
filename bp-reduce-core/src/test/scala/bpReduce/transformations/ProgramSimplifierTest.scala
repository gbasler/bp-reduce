package bpReduce
package transformations

import bpReduce.BaseSpecification
import bpReduce.ast.Program
import bpReduce.reader.BooleanProgramParser

class ProgramSimplifierTest extends BaseSpecification {

  "program simplifier: conservative" should {
    implicit def fromText(program: String) = {
      new BooleanProgramParser().parse(program)
    }

    "only skips" in {
      val program: Program =
        """|void main()
          |begin
          |skip;
          |skip;
          |end
          |
        """.stripMargin

      val simplified: Program =
        """|void main()
          |begin
          |end
          |
        """.stripMargin

      val actual: Program = ProgramSimplifier(program)
      actual must beSameProgram(simplified)
    }

    "only skips and some unused labels" in {
      val program: Program =
        """|void main()
          |begin
          |L1: skip;
          |L2: skip;
          |end
          |
        """.stripMargin

      val simplified: Program =
        """|void main()
          |begin
          |end
          |
        """.stripMargin

      ProgramSimplifier(program) must beSameProgram(simplified)
    }

    "non removable skip as jump target" in {
      val program1: Program =
        """|void main()
          |begin
          |goto L1, L2;
          |L2: assume T;
          |L1: skip;
          |end
          |
        """.stripMargin

      val program2: Program =
        """|void main()
          |begin
          |goto L1, L2;
          |L2: assume T;
          |L1: skip;
          |skip;
          |end
          |
        """.stripMargin

      val program3: Program =
        """|void main()
          |begin
          |goto L1, L2;
          |L2: assume T;
          |skip;
          |L1: skip;
          |end
          |
        """.stripMargin

      val simplified: Program =
        """|void main()
          |begin
          |goto L1, L2;
          |L2: assume T;
          |L1: skip;
          |end
          |
        """.stripMargin

      ProgramSimplifier(program1) must beSameProgram(simplified)
      ProgramSimplifier(program2) must beSameProgram(simplified)
      ProgramSimplifier(program3) must beSameProgram(simplified)
    }

    "more non removable skips as jump target" in {
      val program: Program =
        """|void main()
          |begin
          |goto L1, L2;
          |L2: skip;
          |skip;
          |L1: skip;
          |skip;
          |end
          |
        """.stripMargin

      // a more aggressive optimization could also remove the second skip
      // and then remove the goto as well
      // but that could hide some bugs
      // (e.g., several threads having different pcs
      // since there's only one or zero stmt left)
      val simplified: Program =
        """|void main()
          |begin
          |goto L1, L2;
          |L2: skip;
          |L1: skip;
          |end
          |
        """.stripMargin

      ProgramSimplifier(program) must beSameProgram(simplified)
    }
  }
}
