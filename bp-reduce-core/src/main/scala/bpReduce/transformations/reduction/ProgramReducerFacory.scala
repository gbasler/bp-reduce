package bpReduce
package transformations
package reduction

import bpReduce.ast.Program

trait ProgramReducerFacory {
  def apply(program: Program): ProgramReducer
}
