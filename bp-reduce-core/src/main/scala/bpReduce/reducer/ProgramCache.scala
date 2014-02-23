package bpReduce
package reducer

import bpReduce.ast.Program
import scala.collection.mutable

sealed abstract class CacheState

object CacheState {

  case object Accepted extends CacheState

  case object Rejected extends CacheState

  case object Unknown extends CacheState

}

/**
 * Simple program cache that contains all variants of
 * a program that have been checked.
 *
 * Note that we store positive and negative results, since
 * it might happen that a reduced variant corresponds to a positive
 * checked other variant after program simplification.
 *
 */
class ProgramCache {
  val cache = mutable.Map.empty[Program, CacheState]

  def add(program: Program, state: CacheState) = {
    require(!cache.contains(program))
    cache += program -> state
  }

  def check(program: Program): CacheState = {
    import CacheState._
    cache.get(program) match {
      case Some(state) => state
      case None        => Unknown
    }
  }
}
