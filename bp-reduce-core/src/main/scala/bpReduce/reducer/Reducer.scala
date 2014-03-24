package bpReduce
package reducer

import bpReduce.ast.{Sym, Program}
import scala.annotation.tailrec
import bpReduce.reduction.{ProgramReducer, ProgramReducerFactory}
import bpReduce.transformations.ProgramSimplifier
import scala.collection.immutable.ListMap

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
               reducers: List[ProgramReducerFactory],
               allReducers: Set[ProgramReducerFactory],
               highPriorityReducers: ListMap[ProgramReducerFactory, Set[Sym]] = ListMap(), // list for deterministic results
               current: Option[Program] = None,
               iteration: Int = 0): (Option[Program], Int) = {

      @inline
      def con(factory: ProgramReducerFactory,
              tail: List[ProgramReducerFactory],
              highPriority: ListMap[ProgramReducerFactory, Set[Sym]]) = {

      }

      if (highPriorityReducers.isEmpty) {
        reducers match {
          case Nil             =>
            current -> iteration
          case factory :: tail =>
            con(factory, tail, highPriorityReducers)
        }
      } else {
        con(highPriorityReducers.head._1, reducers, highPriorityReducers.tail)
      }

      reducers match {
        case Nil             =>
          current -> iteration
        case factory :: tail =>
          val reducer = factory(current.getOrElse(original))
          val (variant, iter, rwSyms) = reduceMax(reducer, None, iteration, Set())

          // 1. find all reducers that are influenced by changed symbols
          val influenced = allReducers - factory

          // 2. add reducers to high priority list
          // problem: we need to add them with a filter!!! (since we do not know the statements in advance!!!)
          // thus we need to lazily add them and keep the filter
          val updatedHighPriorityReducers = influenced.foldLeft(highPriorityReducers) {
            case (map, reducer) =>
              val syms = map.get(reducer).map(_ ++ rwSyms).getOrElse(rwSyms)
              map - reducer + (reducer -> syms)
          }

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
          // since they run with a filter, we'd only need to run the inverse filters afterwards...

          // symbols -> reducers

          // if no reduction was possible, we must continue with last possible one
          reduce(original, tail, allReducers, updatedHighPriorityReducers, variant.orElse(current), iter)
      }
    }

    @tailrec
    def reduceUntilFixpoint(program: Program,
                            iteration: Int = 1,
                            fixpoints: Int = 0): Program = {
      reduce(program, config.reducers, config.reducers.toSet, iteration = iteration) match {
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
