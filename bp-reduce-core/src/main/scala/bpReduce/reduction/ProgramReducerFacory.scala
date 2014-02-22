package bpReduce
package reduction

import bpReduce.ast.Program

trait ProgramReducerFacory {
  /**
   *
   * @param program
   * @return A reducer, if at least one reduction is possible,
   *         `None` otherwise.
   */
  def apply(program: Program): Option[ProgramReducer]
}
