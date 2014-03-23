package bpReduce
package reduction

import bpReduce.ast.Stmt
import bpReduce.ast.Stmt._
import bpReduce.ast.Program

object Reducers {

  object ReplaceWithSkip extends ProgramReducerFactory {
    def apply(program: Program) = {
      val skipReducer = new StmtReducerFactory {
        def apply(stmt: Stmt) = {
          // check if reduction really possible,
          // otherwise we'll have an infinite loop
          stmt match {
            case Skip => None
            case _    => Some(new StmtReducer {
              def from: Stmt = stmt

              def to = Skip

              def reduce = None

              def advance = None
            })
          }

        }
      }
      ComposedProgramReducer(skipReducer, program)
    }
  }

  object ReduceAssigns extends ProgramReducerFactory {
    def apply(program: Program): Option[ProgramReducer] = {
      val assignReducer = new StmtReducerFactory {
        def apply(stmt: Stmt): Option[ReduceAssign] = ReduceAssign(stmt)
      }
      ComposedProgramReducer(assignReducer, program)
    }
  }

  val All = List(ReplaceWithSkip, ReduceAssigns, ReduceExpr)
}
