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

    "even more non removable skips as jump target" in {
      val program: Program =
        """|void main()
          |begin
          |l1: PC10:	if F then goto l6; fi;
          |l5: PC39:	goto l1;
          |l6: PC40:	skip;
          |PC41:	skip;
          |end
          |
        """.stripMargin

      val simplified: Program =
        """|void main()
          |begin
          |l1: PC10:	if F then goto l6; fi;
          |l5: PC39:	goto l1;
          |l6: PC40:	skip;
          |end
          |
        """.stripMargin

      ProgramSimplifier(program) must beSameProgram(simplified)
    }

    "assign with constrain T" in {
      val program: Program =
        """|decl g;
          |void main()
          |begin
          |decl l;
          |l, g := g, l constrain T;
          |end
        """.stripMargin

      val simplified: Program =
        """|decl g;
          |void main()
          |begin
          |decl l;
          |l, g := g, l;
          |end
        """.stripMargin

      ProgramSimplifier(program) must beSameProgram(simplified)
    }

    "dead variables" in {
      val program: Program =
        """|decl g;
          |void main() begin
          |	decl b0_4_eq_l;
          |	decl b1_l_eq_s;
          |l1: PC8:	skip;
          |PC9:	start_thread goto l2;
          |PC10:	goto l3;
          |l2: PC11:	skip;
          |PC17:	b1_l_eq_s, b1_l_eq_s$ := T, !b1_l_eq_s$ | b1_l_eq_s;
          |PC18:	assert b1_l_eq_s;
          |l3: PC20:	goto l1;
          |end
          |void c$$__CPROVER_initialize() begin
          |	decl b0_4_eq_l;
          |	decl b1_l_eq_s;
          |end
          |void c$$f() begin
          |	decl b0_4_eq_l;
          |	decl b1_l_eq_s;
          |end
          |void c$$main() begin
          |	decl b0_4_eq_l;
          |	decl b1_l_eq_s;
          |end
        """.stripMargin

      val simplified: Program =
        """|void main() begin
          |	decl b1_l_eq_s;
          |l1: PC8:	skip;
          |PC9:	start_thread goto l2;
          |PC10:	goto l3;
          |l2: PC11:	skip;
          |PC17:	b1_l_eq_s, b1_l_eq_s$ := T, !b1_l_eq_s$ | b1_l_eq_s;
          |PC18:	assert b1_l_eq_s;
          |l3: PC20:	goto l1;
          |end
          |void c$$__CPROVER_initialize() begin
          |end
          |void c$$f() begin
          |end
          |void c$$main() begin
          |end
        """.stripMargin

      ProgramSimplifier.simplifyVariables(program) must beSameProgram(simplified)
    }

    "dead functions" in {
      val program: Program =
        """|void main() begin
          |	decl b0_4_eq_l;
          |	decl b1_l_eq_s;
          |l1: PC8:	skip;
          |PC9:	start_thread goto l2;
          |PC10:	goto l3;
          |l2: PC11:	skip;
          |PC17:	b1_l_eq_s, b1_l_eq_s$ := T, !b1_l_eq_s$ | b1_l_eq_s;
          |PC18:	assert b1_l_eq_s;
          |l3: PC20:	goto l1;
          |end
          |void c$$__CPROVER_initialize() begin
          |	decl b0_4_eq_l;
          |	decl b1_l_eq_s;
          |end
          |void c$$f() begin
          |	decl b0_4_eq_l;
          |	decl b1_l_eq_s;
          |end
          |void c$$main() begin
          |	decl b0_4_eq_l;
          |	decl b1_l_eq_s;
          |end
        """.stripMargin

      val simplified: Program =
        """|void main() begin
          |	decl b0_4_eq_l;
          |	decl b1_l_eq_s;
          |l1: PC8:	skip;
          |PC9:	start_thread goto l2;
          |PC10:	goto l3;
          |l2: PC11:	skip;
          |PC17:	b1_l_eq_s, b1_l_eq_s$ := T, !b1_l_eq_s$ | b1_l_eq_s;
          |PC18:	assert b1_l_eq_s;
          |l3: PC20:	goto l1;
          |end
        """.stripMargin

      ProgramSimplifier.removeDeadFunctions(program) must beSameProgram(simplified)
    }

    "not all dead functions" in {
      val program: Program =
        """|void main() begin
          |	decl b0_4_eq_l;
          |	decl b1_l_eq_s;
          |l1: PC8:	skip;
          |PC9:	start_thread goto l2;
          |PC10:	goto l3;
          |l2: PC11:	skip;
          |PC17:	b1_l_eq_s, b1_l_eq_s$ := c$$__CPROVER_initialize();
          |PC18:	assert b1_l_eq_s;
          |l3: PC20:	goto l1;
          |end
          |bool<2> c$$__CPROVER_initialize() begin
          |	return F, T;
          |end
          |void c$$f() begin
          |	decl b0_4_eq_l;
          |	decl b1_l_eq_s;
          |end
          |void c$$main() begin
          |	decl b0_4_eq_l;
          |	decl b1_l_eq_s;
          |end
        """.stripMargin

      val simplified: Program =
        """|void main() begin
          |	decl b0_4_eq_l;
          |	decl b1_l_eq_s;
          |l1: PC8:	skip;
          |PC9:	start_thread goto l2;
          |PC10:	goto l3;
          |l2: PC11:	skip;
          |PC17:	b1_l_eq_s, b1_l_eq_s$ := c$$__CPROVER_initialize();
          |PC18:	assert b1_l_eq_s;
          |l3: PC20:	goto l1;
          |end
          |bool<2> c$$__CPROVER_initialize() begin
          |	return F, T;
          |end
        """.stripMargin

      ProgramSimplifier.removeDeadFunctions(program) must beSameProgram(simplified)
    }
  }
}
