package bpReduce
package reducer

import bpReduce.ast.{Sym, Program}
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
                  iteration: Int,
                  rwSyms: Set[Sym]): (Option[Program], Int, Set[Sym]) = {
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

              // TODO: prioritize the dependent reducers which should be run immediately after...
              val tainted: Set[Sym] = reducer.rwSyms

              reduceMax(reducer.reduce, Some(simplified), iteration + 1, tainted ++ rwSyms)
            case CheckerResult.Reject =>
              // reduction did not meet criteria
              // check next opportunity
              reduceMax(reducer.advance, lastFeasible, iteration + 1, rwSyms)
          }
        case None          =>
          // reduction not possible, return last feasible reduction
          (lastFeasible, iteration, rwSyms)
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
          val (variant, iter, rwSyms) = reduceMax(reducer, None, iteration, Set())

          // either inner loop, or list...

          // TODO: pull up reducers

          // list of dependent reducers
          // is not easy to create,
          // since we just do not know the exact reducers
          // (a program reducer can contain several stmt reducers, which we don't know in advance)
          // add a filter?


          // case study:
          // - 3 reductions remove assignment stmt
          //

          // idea: list of high priority reducers that are depending on a stmt (expr reducer, stmt remover, etc...)
          // only when that list is finished, we iterate over the rest...
          // problem: when we iterate over the rest, we don't have to run the high priority reducers again...


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
