package bpReduce.frontend

import bpReduce.BaseSpecification

class ParserTest extends BaseSpecification {
  "mini program" in {
    val program =
      """|decl g0;
        |
        |void main()
        |begin
        |skip;
        |end
        |
      """.stripMargin

    val p = new Parser().parse(program)
    ok
  }
}
