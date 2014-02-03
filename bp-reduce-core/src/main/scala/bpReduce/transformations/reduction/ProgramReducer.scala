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
   * @return A [[ProgramReducer]] that can produce the reduced program.
   */
  def reduce: Option[ProgramReducer]

  /**
   * Goes one step towards the right in the lattice. Thus the next
   * program will be an alternative reduction to the one that is
   * now proposed. It will not be simpler though.
   *
   * @return A [[ProgramReducer]] that can produce the reduced program.
   */
  def advance: Option[ProgramReducer]
}