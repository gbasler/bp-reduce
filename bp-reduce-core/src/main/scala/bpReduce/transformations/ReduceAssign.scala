package bpReduce
package transformations

import bpReduce.ast.Program

/**
  */
class ReduceAssign extends Transformer {
  /**
   * @param program Program to transform
   *
   * @return        Updated program, if transformation was possible
   **/
  def transform(program: Program): Option[Program] = {
    // find all locations with assigns

    for {
      f <- program.functions
    } {
    }

    // point iterator to first assign
    //
    ???
  }

  /**
   * @return New transformer with updated internal state for next transformation (if possible).
   */
  def advance(program: Program): Option[Transformer] = {
    // go to next location
    // and create a stream of possible reductions
    ???
  }
}
