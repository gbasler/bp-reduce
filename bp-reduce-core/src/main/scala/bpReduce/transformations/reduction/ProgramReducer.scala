package bpReduce
package transformations
package reduction

import bpReduce.ast.Program

/**
 * Reduces a program.
 */
trait ProgramReducer {

  /**
   * @return current reduction.
   */
  def current: Option[Program]

  /**
   * Goes one step up towards an empty program.
   *
   * Intuitively this means "try to reduce more".
   *
   * @return A [[ProgramReducer]] that can produce the reduced program
   *         or `None` if no reduction is possible.
   */
  def reduce: Option[ProgramReducer]

  /**
   * Goes one step towards the right in the lattice. Thus the next
   * program will be an alternative reduction to the one that is
   * now proposed. It will not be simpler though.
   *
   * Intuitively this means "try to find another opportunity for reduction".
   *
   * @return A [[ProgramReducer]] that can produce the reduced program or
   *         `None` if no reduction is possible.
   */
  def advance: Option[ProgramReducer]

  // TODO: what if advance is called before reduce returns None?
}
