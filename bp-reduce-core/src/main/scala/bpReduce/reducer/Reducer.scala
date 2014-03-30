package bpReduce
package reducer

import bpReduce.ast.{Stmt, Sym, Program}
import scala.annotation.tailrec
import bpReduce.reduction.{StmtFilter, ProgramReducer, ProgramReducerFactory}
import bpReduce.transformations.{VariableCollector, ProgramSimplifier}

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
     * @param filter abbreviate this run by applying the supplied filter
     * @return Reduced program, or `None`, if no reduction possible.
     */
    @tailrec
    def reduce(original: Program,
               reducers: List[ProgramReducerFactory],
               allReducers: Set[ProgramReducerFactory],
               filter: StmtFilter,
               rwSymsAcc: Set[Sym] = Set(),
               current: Option[Program] = None,
               iteration: Int = 0): (Option[Program], Int, Set[Sym]) = {

      reducers match {
        case Nil             =>
          return (current, iteration, rwSymsAcc)
        case factory :: tail =>
          val reducer = factory(current.getOrElse(original), filter)
          val (variant, iter, rwSyms) = reduceMax(reducer, None, iteration, Set())


          // if no reduction was possible, we must continue with last possible one
          reduce(original, tail, allReducers, filter, rwSyms ++ rwSymsAcc, variant.orElse(current), iter)
      }
    }

    @tailrec
    def reduceUntilFixpoint(program: Program,
                            filter: StmtFilter = StmtFilter.Empty,
                            iteration: Int = 1,
                            fixpoints: Int = 0): Program = {
      reduce(program, config.reducers, config.reducers.toSet, filter, iteration = iteration) match {
        case (Some(current), iter, rwSyms) =>
          // reduction was possible, try all reductions again

          if (rwSyms.isEmpty) {
            println("*** next fixpoint iteration ***")
            reduceUntilFixpoint(current, StmtFilter.Empty, iter, fixpoints + 1)
          } else {
            println("*** next quick fixpoint iteration ***")
            val filter = new StmtFilter {
              def filter(stmt: Stmt): Boolean = {
                (VariableCollector(stmt) union rwSyms).nonEmpty
              }
            }
            reduceUntilFixpoint(current, filter, iter, fixpoints + 1)
          }

        case (None, iter, _) if filter != StmtFilter.Empty =>
          // all reducers have been applied but
          // we had a quick run
          reduceUntilFixpoint(program, StmtFilter.Empty, iter, fixpoints + 1)
        case (None, _, _)                                  =>
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
