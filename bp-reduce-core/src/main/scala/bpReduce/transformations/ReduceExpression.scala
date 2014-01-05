package bpReduce
package transformations

import bpReduce.ast.Program

class ReduceExpression extends Transformer {
  /**
   * @param program Program to transform
   *
   * @return        Updated program, if transformation was possible
   **/
  def transform(program: Program): Option[Program] = {
    // find all locations with expressions
  }

  /**
   * @return New transformer with updated internal state for next transformation (if possible).
   */
  def advance(program: Program): Option[Transformer] = {
    // go to next location
    // and create a stream of possible reductions
  }
}
