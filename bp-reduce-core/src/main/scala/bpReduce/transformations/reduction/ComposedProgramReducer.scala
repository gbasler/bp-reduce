package bpReduce
package transformations
package reduction

import bpReduce.ast.Program

case class ComposedProgramReducer(stmtReducer: StmtReducer,
                                  program: Program) extends ProgramReducer {

  override def current = ???

  override def reduce = ???

  override def advance = ???
}
