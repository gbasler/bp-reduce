package bpReduce
package reducer

import bpReduce.ast.Program
import bpReduce.reducer.CheckerResult.{Reject, Accept}

/**
 * Adapter that wraps a [[Checker]] with a transparent cache.
 */
class CachingChecker(checker: Checker) extends Checker {

  private val cache = new ProgramCache

  def apply(program: Program): CheckerResult = {
    cache.check(program) match {
      case CacheState.Accepted =>
        // variant already checked and was ok
        CheckerResult.Accept
      case CacheState.Rejected =>
        // variant already checked and it failed
        CheckerResult.Reject
      case CacheState.Unknown  =>
        val result = checker(program)
        val translated = result match {
          case Accept => CacheState.Accepted
          case Reject => CacheState.Rejected
        }
        cache.add(program, translated)
        result
    }
  }
}
