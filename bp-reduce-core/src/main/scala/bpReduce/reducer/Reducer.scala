package bpReduce
package reducer

import bpReduce.ast.Program
import scala.annotation.tailrec
import bpReduce.reduction.{ProgramReducer, ProgramReducerFacory}
import bpReduce.transformations.ProgramSimplifier

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
    def reduceMax(reducerOpt: Option[ProgramReducer],
                  lastFeasible: Option[Program],
                  iteration: Int): (Option[Program], Int) = {
      reducerOpt match {
        case Some(reducer) =>
          val variant = reducer.current
          val simplified = if (config.simplify) {
            ProgramSimplifier(variant)
          } else {
            variant
          }
          checker(simplified, iteration) match {
            case CheckerResult.Accept =>
              // reduction was accepted
              // continue with (simplified) variant
              reduceMax(reducer.reduce, Some(simplified), iteration + 1)
            case CheckerResult.Reject =>
              // reduction did not meet criteria
              // check next opportunity
              reduceMax(reducer.advance, lastFeasible, iteration + 1)
          }
        case None          =>
          // reduction not possible, return last feasible reduction
          lastFeasible -> iteration
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
               current: Option[Program] = None,
               iteration: Int = 0): (Option[Program], Int) = {
      reducers match {
        case Nil             =>
          current -> iteration
        case factory :: tail =>
          val reducer = factory(current.getOrElse(original))
          val (variant, iter) = reduceMax(reducer, None, iteration)
          // if no reduction was possible, we must continue with last possible one
          reduce(original, tail, variant.orElse(current), iter)
      }
    }

    @tailrec
    def reduceUntilFixpoint(program: Program,
                            iteration: Int = 1,
                            fixpoints: Int = 0): Program = {
      reduce(program, config.reducers, iteration = iteration) match {
        case (Some(current), iter) =>
          // reduction was possible, try all reductions again
          println("*** next fixpoint iteration ***")
          reduceUntilFixpoint(current, iter, fixpoints + 1)
        case (None, _)             =>
          // all reducers have been applied but
          // no reduction was possible, so fixed point reached
          program
      }
    }

    val reduced = reduceUntilFixpoint(program)
    if (config.simplify) {
      ProgramSimplifier(reduced)
    } else {
      reduced
    }

  }
}
