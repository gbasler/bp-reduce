package bpReduce.frontend

import bpReduce.BaseSpecification
import bpReduce.ast.{Function, Program, Stmt}

class ParserTest extends BaseSpecification {
  "empty program" in {
    val program =
      """|void main()
        |begin
        |end
        |
      """.stripMargin

    val parser = new BooleanProgramParser()
    val parsed: Program = parser.parse(program)
    parsed must beLike {
      case Program(_, Seq(Function("main", _, Seq(), 0, Seq()))) => ok
    }
  }

  "skip program" in {
    val program =
      """|void main()
        |begin
        |skip;
        |end
        |
      """.stripMargin

    val parser = new BooleanProgramParser()
    val parsed: Program = parser.parse(program)
    parsed must beLike {
      case Program(_, Seq(Function("main", _, Seq(), 0, Seq(Stmt.Skip)))) => ok
    }
  }

  "skip stmt" in {
    val program = """skip""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Stmt.Skip, _) => ok
    }
  }

  "labelled skip stmt" in {
    val program = """L1: skip""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Stmt.Skip, _) => ok
    }
  }

  "skip program" in {
    val program =
      """|void main()
        |begin
        |skip;
        |end
        |
      """.stripMargin

    val p = new BooleanProgramParser().parse(program)
    ok
  }

  "assign program" in {
    val program =
      """|decl g0;
        |
        |void main()
        |begin
        |g0 := T;
        |end
        |
      """.stripMargin

    val p = new BooleanProgramParser().parse(program)
    ok
  }
}
