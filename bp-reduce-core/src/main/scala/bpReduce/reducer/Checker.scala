package bpReduce
package reducer

import bpReduce.ast.Program

sealed abstract class CheckerResult

object CheckerResult {

  /** candidate program is interesting and passed
    * validity checks */
  object Accept extends CheckerResult

  /** candidate program did not pass the test */
  object Reject extends CheckerResult

}

trait Checker {
  def apply(program: Program): CheckerResult
}
