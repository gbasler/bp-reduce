package bpReduce
package transformations
package reduction

import org.specs2.mutable.Specification
import bpReduce.ast._
import bpReduce.ast.Stmt.{AtomicEnd, EndThread, Skip, Return}
import bpReduce.ast.VariableHolder
import bpReduce.ast.Function
import bpReduce.ast.Program
import bpReduce.reader.BooleanProgramParser
import bpReduce.writer.Formatter

class ComposedProgramReducerTest extends BaseSpecification {
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

      val reducer = ComposedProgramReducer(factory, program).get
      reducer.current.get must be_==(skip)
      reducer.reduce must beNone
      reducer.advance must beNone
    }

    "complex one line program" in {
      // now the fun starts...
      val factory = new StmtReducerFactory {
        override def create(stmt: Stmt): StmtReducer = new StmtReducer {

          override def current = Some(Skip)

          override def reduce = Some(new StmtReducer {

            override def current = Some(AtomicEnd)

            override def reduce = None

            override def advance = None
          })


          override def advance = Some(new StmtReducer {

            override def current = Some(EndThread)

            override def reduce = None

            override def advance = None
          })

        }
      }

      val program: Program =
        """|void main()
          |begin
          |return;
          |end
          |
        """.stripMargin

      val reducedToSkip: Program =
        """|void main()
          |begin
          |skip;
          |end
          |
        """.stripMargin

      val reducedFurtherToAtomicEnd: Program =
        """|void main()
          |begin
          |atomic_end;
          |end
          |
        """.stripMargin

      val endThread: Program =
        """|void main()
          |begin
          |end_thread;
          |end
          |
        """.stripMargin

      val reducer = ComposedProgramReducer(factory, program).get
      reducer.current.get must be_==(reducedToSkip)
      reducer.reduce.get.current.get must be_==(reducedFurtherToAtomicEnd)
      reducer.advance.get.current.get must be_==(endThread)
      reducer.advance.get.reduce must beNone
    }

    "two line program" in {
      // even more fun ...
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
          |return;
          |end
          |
        """.stripMargin

      val skipFirst: Program =
        """|void main()
          |begin
          |skip;
          |return;
          |end
          |
        """.stripMargin

      val skipLast: Program =
        """|void main()
          |begin
          |return;
          |skip;
          |end
          |
        """.stripMargin

      val skipTwice: Program =
        """|void main()
          |begin
          |skip;
          |skip;
          |end
          |
        """.stripMargin

      val reducer = ComposedProgramReducer(factory, program).get
      reducer.current.get === skipFirst
      reducer.advance.get.current.get must beSameProgram(skipLast)
      reducer.reduce.get.current.get must beSameProgram(skipTwice)
    }
  }
}
