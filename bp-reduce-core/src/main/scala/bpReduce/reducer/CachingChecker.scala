package bpReduce
package reducer

import bpReduce.ast.Program
import bpReduce.reducer.CheckerResult.{Reject, Accept}

/**
 * Adapter that wraps a [[Checker]] with a transparent cache.
 */
class CachingChecker(checker: Checker,
                     verbose: Boolean,
                     cache: ProgramCache = new ProgramCache) extends Checker {

  def apply(program: Program,
            iteration: Int): CheckerResult = {

    @inline
    def formatFileName(fileName: Option[String]) = {
      fileName.filter(_ => verbose).map(" <" + _ + ">").getOrElse("")
    }

    cache.check(program) match {
      case (CacheState.Accepted, fileName) =>
        // variant already checked and was ok
        println(s"[$iteration] (cache${formatFileName(fileName)}) : √")
        CheckerResult.Accept
      case (CacheState.Rejected, fileName) =>
        // variant already checked and it failed
        println(s"[$iteration] (cache${formatFileName(fileName)}) : -")
        CheckerResult.Reject
      case (CacheState.Unknown, _)         =>
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
