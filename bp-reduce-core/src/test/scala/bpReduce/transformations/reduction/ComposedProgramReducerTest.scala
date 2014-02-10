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

      val program = Program(VariableHolder(), Seq(Function("main")))
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
          |return;
          |end
          |
        """.stripMargin

      val reducer: ComposedProgramReducer = ComposedProgramReducer(factory, program).get
      val s = reducer.current
      reducer.current must beSome(skip)

    }
  }
}
