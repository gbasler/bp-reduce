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

      val (factory, tail, highPriority) = if (highPriorityReducers.isEmpty) {
        reducers match {
          case Nil             =>
            return current -> iteration
          case factory :: tail =>
            (factory, tail, highPriorityReducers)
        }
      } else {
        (highPriorityReducers.head._1, reducers, highPriorityReducers.tail)
      }

      val reducer = factory(current.getOrElse(original))
      val (variant, iter, rwSyms) = reduceMax(reducer, None, iteration, Set())

      // 1. find all reducers that are influenced by changed symbols
      val influenced = allReducers - factory

      // 2. add reducers to high priority list
      // problem: we need to add them with a filter!!! (since we do not know the statements in advance!!!)
      // thus we need to lazily add them and keep the filter
      val updatedHighPriorityReducers = influenced.foldLeft(highPriority) {
        case (map, reducer) =>
          val syms = map.get(reducer).map(_ ++ rwSyms).getOrElse(rwSyms)
          map - reducer + (reducer -> syms)
      }

      // if no reduction was possible, we must continue with last possible one
      reduce(original, tail, allReducers, updatedHighPriorityReducers, variant.orElse(current), iter)
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
