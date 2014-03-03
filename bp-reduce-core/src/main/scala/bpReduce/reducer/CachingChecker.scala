package bpReduce
package reducer

import bpReduce.ast.Program
import bpReduce.reducer.CheckerResult.{Reject, Accept}

/**
 * Adapter that wraps a [[Checker]] with a transparent cache.
 */
class CachingChecker(checker: Checker) extends Checker {

  private val cache = new ProgramCache

  def apply(program: Program,
            iteration: Int): CheckerResult = {
    cache.check(program) match {
      case CacheState.Accepted =>
        // variant already checked and was ok
        println(s"(cache): accepted.")
        CheckerResult.Accept
      case CacheState.Rejected =>
        // variant already checked and it failed
        println(s"(cache): rejected.")
        CheckerResult.Reject
      case CacheState.Unknown  =>
        val result = checker(program, iteration)
        val translated = result match {
          case Accept => CacheState.Accepted
          case Reject => CacheState.Rejected
        }
        cache.add(program, translated)
        result
    }
  }
}
