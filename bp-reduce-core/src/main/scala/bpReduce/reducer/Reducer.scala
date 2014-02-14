package bpReduce
package reducer

import bpReduce.ast.Program
import scala.annotation.tailrec
import bpReduce.transformations.reduction.{ProgramReducer, ProgramReducerFacory}
import bpReduce.reducer.CheckerResult.{Reject, Accept}

final case class Reducer(config: ReducerConfig) {
  /**
   * Main loop, corresponds to algorithm in the c-reduce paper.
   * @param program to reduce
   */
  def apply(program: Program): Program = {
    import config.checker

    /**
     * Reduce program as much as possible, given the checker and
     * a program reduction.
     */
    @tailrec
    def reduceMax(reducer: Option[ProgramReducer],
                  lastFeasible: Option[Program]): Option[Program] = {
      val possibleVariant = reducer.flatMap(_.current)
      possibleVariant match {
        case None          =>
          // reduction not possible, return last feasible reduction
          lastFeasible
        case Some(variant) =>
          checker(variant) match {
            case Accept =>
              // reduction was accepted
              // continue with variant
              reduceMax(reducer.flatMap(_.reduce), lastFeasible)
            case Reject =>
              // reduction did not meet criteria
              // check next opportunity
              reduceMax(reducer.flatMap(_.advance), None)
          }
      }
    }

    /**
     * Applies successively all reductions that are given.
     *
     * @return Reduced program, or `None`, if no reduction possible.
     */
    @tailrec
    def reduce(original: Program,
               reducers: List[ProgramReducerFacory],
               current: Option[Program] = None): Option[Program] = {
      reducers match {
        case Nil             =>
          current
        case factory :: tail =>
          val reducer = factory(current.getOrElse(original))
          val variant = reduceMax(Some(reducer), None)
          reduce(original, tail, variant)
      }
    }

    @tailrec
    def reduceUntilFixpoint(program: Program): Program = {
      reduce(program, config.reducers) match {
        case Some(current) =>
          // reduction was possible, try all reductions again
          reduceUntilFixpoint(current)
        case None          =>
          // no reduction possible, fixed point reached
          program
      }
    }

    reduceUntilFixpoint(program)
  }
}
