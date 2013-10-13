package bpReduce.frontend

import bpReduce.BaseSpecification
import bpReduce.ast._
import bpReduce.ast.VariableHolder
import bpReduce.ast.Function
import bpReduce.ast.Program

class ParserTest extends BaseSpecification {
  "variables" should {
    "alpha vars" in {
      val program = """decl g;""".stripMargin
      val parser = new BooleanProgramParser()
      parser.parseAll(parser.decl, program) must beLike {
        case parser.Success(List(Sym("g")), _) => ok
      }
    }

    "alpha / digit vars" in {
      val program = """decl g0;""".stripMargin
      val parser = new BooleanProgramParser()
      parser.parseAll(parser.decl, program) must beLike {
        case parser.Success(List(Sym("g0")), _) => ok
      }
    }
  }

  "skip stmt" in {
    val program = """skip""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Stmt.Skip, _) => ok
    }
  }

  "assign stmt" in {
    val program = """g := T""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Stmt.Assign(Seq((Sym("g"), Expr.True))), _) => ok
    }
  }

  "labelled skip stmt" in {
    val program = """L1: skip""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Stmt.Skip, _) => ok
    }
  }

  "empty program" in {
    val program =
      """|void main()
        |begin
        |end
        |
      """.stripMargin

    new BooleanProgramParser().parse(program) must beLike {
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

    new BooleanProgramParser().parse(program) must beLike {
      case Program(_, Seq(Function("main", _, Seq(), 0, Seq(Stmt.Skip)))) => ok
    }
  }

  "program with global variable" in {
    val program =
      """decl g;
        |
        |void main()
        |
        |begin
        |end
        |
      """.stripMargin

    val expected = Program(VariableHolder(Seq(Sym("g"))), Seq(Function("main", VariableHolder(Seq()), Seq(), 0, Seq())))
    new BooleanProgramParser().parse(program) must be_==(expected)
  }

  "program with local variable" in {
    val program =
      """|void main()
        |begin
        |decl l;
        |end
        |
      """.stripMargin

    val expected = Program(VariableHolder(Seq()), Seq(Function("main", VariableHolder(Seq(Sym("l"))), Seq(), 0, Seq())))
    new BooleanProgramParser().parse(program) must be_==(expected)
  }

  "skip program" in {
    val program =
      """|void main()
        |begin
        |skip;
        |end
        |
      """.stripMargin

    val expected = Program(VariableHolder(Seq()), Seq(Function("main", VariableHolder(Seq()), Seq(), 0, Seq(Stmt.Skip))))
    new BooleanProgramParser().parse(program) must be_==(expected)
  }

  "assign program" in {
    val program =
      """|decl g;
        |
        |void main()
        |begin
        |g := T;
        |end
        |
      """.stripMargin

    val expected = Program(VariableHolder(Seq(Sym("g"))), Seq(Function("main", VariableHolder(Seq()), Seq(), 0, Seq(Stmt.Assign(Seq((Sym("g"), Expr.True)))))))
    new BooleanProgramParser().parse(program) must be_==(expected)
  }
}
