package bpReduce
package transformations
package reduction

import bpReduce.ast.Program

case class ComposedProgramReducer(stmtReducer: StmtReducer,
                                  program: Program) extends ProgramReducer {

  // TODO: this will not work for object Stmts! need to find another way...
  val candidates = program.filter(stmtReducer.isDefinedAt)

  override def current = ???

  override def reduce = ???

  override def advance = ???
}
