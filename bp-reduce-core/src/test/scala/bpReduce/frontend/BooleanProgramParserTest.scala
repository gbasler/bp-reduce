package bpReduce.frontend

import bpReduce.BaseSpecification
import bpReduce.ast._
import bpReduce.ast.VariableHolder
import bpReduce.ast.Function
import bpReduce.ast.Program
import bpReduce.ast.Expr._
import bpReduce.ast.Stmt._

class BooleanProgramParserTest extends BaseSpecification {

  import StateIdentifier._
  import MixedIdentifier._

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

  "illegal stmt" in {
    val program = """error""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Failure(_, _) => ok // TODO: better error message
    }
  }

  "skip" in {
    val program = """skip""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Skip, _) => ok
    }
  }

  "assign" in {
    val program = """g := T""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Assign(Seq((Var(Sym("g"), Current, NonMixed), True)), None), _) => ok
    }
  }

  "assign with constrain stmt" in {
    val program = """g := * constrain('g)""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Assign(Seq((Var(Sym("g"), Current, NonMixed), Nondet)), Some(Var(Sym("g"), Next, NonMixed))), _) => ok
    }
  }

  "parallel assign with constrain" in {
    val program = """g1, g0 := *, * constrain('g1 != 'g0)""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Assign(Seq((Var(Sym("g1"), Current, NonMixed), Nondet), (Var(Sym("g0"), Current, NonMixed), Nondet)), Some(Xor(Var(Sym("g1"), Next, NonMixed), Var(Sym("g0"), Next, NonMixed)))), _) => ok
    }
  }

  "parallel assign with mixed variables" in {
    val program = """$l1, l0 := *, * constrain('$l1 != 'l0)""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Assign(Seq((Var(Sym("l1"), Current, Mixed), Nondet), (Var(Sym("l0"), Current, NonMixed), Nondet)), Some(Xor(Var(Sym("l1"), Next, Mixed), Var(Sym("l0"), Next, NonMixed)))), _) => ok
    }
  }

  "function call" in {
    val program = """error_1()""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Call("error_1", Seq(), Seq()), _) => ok
    }
  }

  "function call with args" in {
    val program = """error_1(g != l)""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Call("error_1", Seq(), Seq(Xor(Var(Sym("g"), Current, NonMixed), Var(Sym("l"), Current, NonMixed)))), _) => ok
    }
  }

  "function call with assign" in {
    val program = """a, _, b := foo()""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Call("foo", Seq(Some(Sym("a")), None, Some(Sym("b"))), Seq()), _) => ok
    }
  }

  "dead" in {
    val program = """dead g, l""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Dead(Seq(Sym("g"), Sym("l"))), _) => ok
    }
  }

  "assume" in {
    val program = """assume(g & l)""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Assume(And(Var(Sym("g"), Current, NonMixed), Var(Sym("l"), Current, NonMixed))), _) => ok
    }
  }

  "assume with wrong expr" in {
    val program = """assume(T & 42)""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Failure(msg, _) => ok // TODO: better error message
    }
  }

  "assert" in {
    val program = """assert(g & l)""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Assert(And(Var(Sym("g"), Current, NonMixed), Var(Sym("l"), Current, NonMixed))), _) => ok
    }
  }

  "if" in {
    val program = """if 0 then goto l6; fi""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(If(False, Seq(Goto(Seq("l6"))), Seq()), _) => ok
    }
  }

  "if-else" in {
    val program = """if 0 then goto l6; else goto l7; fi""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(If(False, Seq(Goto(Seq("l6"))), Seq(Goto(Seq("l7")))), _) => ok
    }
  }

  "if-elif" in {
    val program = """if 0 then goto l6; elif 1 then goto l7; fi""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(If(False, Seq(Goto(Seq("l6"))), Seq(If(True, Seq(Goto(Seq("l7"))), Seq()))), _) => ok
    }
  }

  "if-elif-else: dangling else?" in {
    val program = """if 0 then goto l6; elif 1 then goto l7; else goto l8; fi""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(If(False, Seq(Goto(Seq("l6"))), Seq(If(True, Seq(Goto(Seq("l7"))), Seq(Goto(Seq("l8")))))), _) => ok
    }
  }

  "if-elif-else: correct ordering" in {
    val program = """if 0 then goto l6; elif 1 then goto l7; elif 0 then goto l8; else goto l9; fi""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(If(False, Seq(Goto(Seq("l6"))), Seq(If(True, Seq(Goto(Seq("l7"))), Seq(If(False, Seq(Goto(Seq("l8"))), Seq(Goto(Seq("l9")))))))), _) => ok
    }
  }

  "goto" in {
    val program = """goto L1, L2""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Goto(Seq("L1", "L2")), _) => ok
    }
  }

  "return" in {
    val program = """return g & l, T""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Return(Seq(And(Var(Sym("g"), Current, NonMixed), Var(Sym("l"), Current, NonMixed)), True)), _) => ok
    }
  }

  "labelled skip" in {
    val program = """L1: skip""".stripMargin
    val parser = new BooleanProgramParser()
    parser.parseAll(parser.labelledStmt, program) must beLike {
      case parser.Success(Skip, _) => ok
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
      case Program(_, Seq(Function("main", _, Seq(), 0, Seq(Skip)))) => ok
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

    val expected = Program(VariableHolder(Seq()), Seq(Function("main", VariableHolder(Seq()), Seq(), 0, Seq(Skip))))
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

    val expected = Program(VariableHolder(Seq(Sym("g"))), Seq(Function("main", VariableHolder(Seq()), Seq(), 0, Seq(Assign(Seq((Var(Sym("g"), Current, NonMixed), Expr.True)), None)))))
    new BooleanProgramParser().parse(program) must be_==(expected)
  }
}
