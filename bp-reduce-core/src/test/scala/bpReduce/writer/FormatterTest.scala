package bpReduce
package writer

import bpReduce.reader.BooleanProgramParser
import bpReduce.{Resources, BaseSpecification}
import bpReduce.ast.Stmt.Assign

class FormatterTest extends BaseSpecification {
  "eat your own dogfood" in {
    val content = Resources.loadFileOrUrl("trace_WP_bug2/main.bp")
    val program = new BooleanProgramParser().parse(content)
    val prettyContent = Formatter.format(program)
    val reparsed = new BooleanProgramParser().parse(prettyContent)
    program === reparsed
  }

  "missing in action" in {
    val content = Resources.loadFileOrUrl("missing-in-action.bp")
    val program = new BooleanProgramParser().parse(content)
    val prettyContent = Formatter.format(program)
    val reparsed = new BooleanProgramParser().parse(prettyContent)
    program === reparsed
  }

  "complex expression" in {
    implicit def assignFromString(str: String): Assign = {
      new BooleanProgramParser().parseAssign(str)
    }

    val assign: Assign =
      """|b3_l_eq_s,b4_0_eq_l,b5_1_eq_l := *,*,* constrain
        |    ((!b0_s_le_2) | (!b3_l_eq_s) | 'b3_l_eq_s | (!b4_0_eq_l) | (!'b4_0_eq_l)) &
        |    (b0_s_le_2 | (!'b3_l_eq_s) | (!'b4_0_eq_l)) &
        |    ((!b0_s_le_2) | (!b3_l_eq_s) | (!'b3_l_eq_s) | b4_0_eq_l | (!'b4_0_eq_l)) &
        |    ((!b0_s_le_2) | (!(b3_l_eq_s$)) | (!'b3_l_eq_s) | (b4_0_eq_l$) | (!'b4_0_eq_l))
      """.stripMargin
    val actual = Formatter.format(assign)
    val expected = "b3_l_eq_s, b4_0_eq_l, b5_1_eq_l := *, *, * constrain (!b0_s_le_2 | !b3_l_eq_s | 'b3_l_eq_s | !b4_0_eq_l | !'b4_0_eq_l) & (b0_s_le_2 | !'b3_l_eq_s | !'b4_0_eq_l) & (!b0_s_le_2 | !b3_l_eq_s | !'b3_l_eq_s | b4_0_eq_l | !'b4_0_eq_l) & (!b0_s_le_2 | !b3_l_eq_s$ | !'b3_l_eq_s | b4_0_eq_l$ | !'b4_0_eq_l)"
    actual === expected
  }
}