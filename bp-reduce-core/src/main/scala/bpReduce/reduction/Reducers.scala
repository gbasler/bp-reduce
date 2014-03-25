package bpReduce
package reduction

import bpReduce.ast.{Sym, Stmt, Program}
import bpReduce.ast.Stmt._

object Reducers {

  case object ReplaceWithSkip extends ProgramReducerFactory {
    def apply(program: Program,
              filter: Option[Set[Sym]]) = {
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
      ComposedProgramReducer(skipReducer, program, filter)
    }
  }

  case object ReduceAssigns extends ProgramReducerFactory {
    def apply(program: Program, filter: Option[Set[Sym]]): Option[ProgramReducer] = {
      val assignReducer = new StmtReducerFactory {
        def apply(stmt: Stmt): Option[ReduceAssign] = ReduceAssign(stmt)
      }
      ComposedProgramReducer(assignReducer, program, filter)
    }
  }

  val All = List(ReplaceWithSkip, ReduceAssigns, ReduceExpr)
}
