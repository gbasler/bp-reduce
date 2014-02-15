package bpReduce
package transformations
package reduction

import bpReduce.ast.Stmt
import bpReduce.ast.Stmt._
import scala.Some
import bpReduce.ast.Program

object Reducers {

  object ReplaceWithSkip extends ProgramReducerFacory {
    def apply(program: Program) = {
      val skipReducer = new StmtReducerFactory {
        def apply(stmt: Stmt): StmtReducer = new StmtReducer {

          def current = {
            // check if reduction really possible,
            // otherwise we'll have an infinite loop
            stmt match {
              case Skip => None
              case _    => Some(Skip)
            }
          }

          def reduce = None

          def advance = None
        }
      }
      ComposedProgramReducer(skipReducer, program)
    }
  }

  object ReduceAssigns extends ProgramReducerFacory {
    def apply(program: Program): Option[ProgramReducer] = {
      val assignReducer = new StmtReducerFactory {
        def apply(stmt: Stmt): StmtReducer = ReduceAssign(stmt)
      }
      ComposedProgramReducer(assignReducer, program)
    }
  }

  val All = List(ReplaceWithSkip, ReduceAssigns, ReduceExpressions)
}
