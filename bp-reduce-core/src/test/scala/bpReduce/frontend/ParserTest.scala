package bpReduce.frontend

import bpReduce.BaseSpecification
import bpReduce.ast._
import bpReduce.ast.VariableHolder
import bpReduce.ast.Function
import bpReduce.ast.Program
import bpReduce.ast.Expr.{True, Var, And}

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

  "dead stmt" in {
    val program = """dead g, l""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Stmt.Dead(Seq(Sym("g"), Sym("l"))), _) => ok
    }
  }

  "assume stmt" in {
    val program = """assume(g & l)""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Stmt.Assume(And(Var(Sym("g")), Var(Sym("l")))), _) => ok
    }
  }

  "assert stmt" in {
    val program = """assert(g & l)""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Stmt.Assert(And(Var(Sym("g")), Var(Sym("l")))), _) => ok
    }
  }

  "return stmt" in {
    val program = """return g & l, T""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Stmt.Return(Seq(And(Var(Sym("g")), Var(Sym("l"))), True)), _) => ok
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
