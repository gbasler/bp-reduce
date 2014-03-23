package bpReduce
package reduction

import bpReduce.ast.Program

/**
 * Note: these guys are added to a map / set, so derived classed should be case classes
 */
trait ProgramReducerFactory {
  /**
   *
   * @param program
   * @return A reducer, if at least one reduction is possible,
   *         `None` otherwise.
   */
  def apply(program: Program): Option[ProgramReducer]
}
