package bpReduce
package transformations
package reduction

import org.specs2.mutable.Specification
import bpReduce.ast._
import bpReduce.ast.Stmt.{Skip, Return}
import bpReduce.ast.VariableHolder
import bpReduce.ast.Function
import bpReduce.ast.Program
import bpReduce.reader.BooleanProgramParser

class ComposedProgramReducerTest extends Specification {
  "reduce" should {

    implicit def fromText(program: String) = {
      new BooleanProgramParser().parse(program)
    }

    "empty program" in {
      val factory = new StmtReducerFactory {
        override def create(stmt: Stmt): StmtReducer = ???
      }

      val program = Program(VariableHolder(), List(Function("main")))
      ComposedProgramReducer(factory, program) must beNone
    }

    "one line program" in {
      val factory = new StmtReducerFactory {
        override def create(stmt: Stmt): StmtReducer = new StmtReducer {

          override def current = Some(Skip)

          override def reduce = None

          override def advance = None
        }
      }

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

      val reducer: ComposedProgramReducer = ComposedProgramReducer(factory, program).get
      reducer.current.get must be_==(skip)
      reducer.reduce must beNone
      reducer.advance must beNone
    }

    "complex one line program" in {
      // now the fun starts...
      val factory = new StmtReducerFactory {
        override def create(stmt: Stmt): StmtReducer = new StmtReducer {

          override def current = Some(Skip)

          override def reduce = None

          override def advance = None
        }
      }

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

      val reducer: ComposedProgramReducer = ComposedProgramReducer(factory, program).get
      reducer.current.get must be_==(skip)
      reducer.reduce must beNone
      reducer.advance must beNone
      ok
    }

    "two line program" in {
      // now the fun starts...
      ok
    }
  }
}
