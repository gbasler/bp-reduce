package bpReduce
package transformations
package reduction

import bpReduce.ast.Stmt
import bpReduce.ast.Stmt._
import scala.Some
import bpReduce.ast.Program

object Reducers {
  val ReplaceWithSkip = new ProgramReducerFacory {
    override def apply(program: Program) = {
      val skipReducer = new StmtReducerFactory {
        override def create(stmt: Stmt): StmtReducer = new StmtReducer {

          override def current = {
            // check if reduction really possible,
            // otherwise we'll have an infinite loop
            stmt match {
              case Skip => None
              case _    => Some(Skip)
            }

          }

          override def reduce = None

          override def advance = None
        }
      }
      ComposedProgramReducer(skipReducer, program)
    }
  }
}
