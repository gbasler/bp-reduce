package bpReduce
package reducer

trait OutputChecker {
  def apply(output: String): CheckerResult
}

final case class ErrorOutputChecker(errorLine: String) extends OutputChecker {
  def apply(output: String): CheckerResult = {
    if (output.contains(errorLine)) {
      CheckerResult.Accept
    } else {
      CheckerResult.Reject
    }
  }
}

object ErrorOutputChecker {
  val Default = ErrorOutputChecker("Assertion failed")
}