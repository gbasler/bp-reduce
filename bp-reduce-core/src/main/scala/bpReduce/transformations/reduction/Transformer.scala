package bpReduce
package transformations
package reduction

import bpReduce.ast.Program

/**
 * Actually more a state than a transformer
 */
trait Transformer {
  /**
   * @param program Program to transform
   *
   * @return        Updated program, if transformation was possible
   **/
  def transform(program: Program): Option[Program]

  /**
   * @return New transformer with updated internal state for next transformation (if possible).
   */
  def advance(program: Program): Option[Transformer]
}
