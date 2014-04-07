package bpReduce
package reducer

import bpReduce.ast.{Sym, Program}
import scala.annotation.tailrec
import bpReduce.reduction.{InfluencedBySymbolFilter, StmtFilter, ProgramReducer, ProgramReducerFactory}
import bpReduce.transformations.ProgramSimplifier

sealed abstract class Run

object Run {

  case object Full extends Run

  final case class Quick(filter: StmtFilter) extends Run

  final case class QuickRemaining(filter: StmtFilter) extends Run

}

final case class Reducer(config: ReducerConfig, verbose: Boolean) {
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
          if (verbose) {
            println(s"Checking ${reducer.currentComment}")
          }
          checker(simplified, iteration) match {
            case CheckerResult.Accept =>
              // reduction was accepted
              // continue with (simplified) variant

              // record symbols that influence or are influenced by current stmt
              // in order to run only dependent reductions in next iteration
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
          (current, iteration, rwSymsAcc)
        case factory :: tail =>
          val reducer = factory(current.getOrElse(original), filter)
          val (variant, iter, rwSyms) = reduceMax(reducer, None, iteration, Set())

          // if no reduction was possible, we must continue with last possible one
          reduce(original, tail, allReducers, filter, rwSyms ++ rwSymsAcc, variant.orElse(current), iter)
      }
    }

    @tailrec
    def reduceUntilFixpoint(program: Program,
                            run: Run,
                            iteration: Int = 1,
                            fixpoints: Int = 0): Program = {

      import Run._

      val filter = run match {
        case Full                   => StmtFilter.Empty
        case Quick(filter)          => filter
        case QuickRemaining(filter) => filter.negate
      }

      reduce(program, config.reducers, config.reducers.toSet, filter, iteration = iteration) match {
        case (Some(current), iter, rwSyms) =>
          // reduction was possible, try all reductions again
          if (rwSyms.isEmpty) {
            run match {
              case Quick(f) if config.smartAcceleration          =>
                // all reducers that were influenced by a variable have been applied but
                // this time no variable was influenced => take shortcut
                println("*** next remaining fixpoint iteration ***")
                reduceUntilFixpoint(program, QuickRemaining(f), iter, fixpoints + 1)
              case QuickRemaining(_) if config.smartAcceleration =>
                // we just had a quick remaining run that reduced something
                // however, no stmts that influence or are influenced by
                // symbols were reduced
                // so what's left?
                // atomic_begin / atomic_end / thread_start
                // let's say that we removed one of these...
                // the first iteration does a full reduction
                // what was it not removed then?
                // removing a start_thread stmt reduces behavior significantly
                // usually a thread needs the help of another thread to reach a certain state
                // if the other thread is missing, it can't reach that state anymore
                // if it can still reach that state then the other thread was not needed
                println("*** next we just had a quick that did not remove any stmt that ***")
                println("*** influences or is influenced by a stmt, so I'm stopping here ***")
                program
              case Full | _ if !config.smartAcceleration         =>
                println("*** next fixpoint iteration ***")
                reduceUntilFixpoint(current, Full, iter, fixpoints + 1)
            }
          } else {
            println("*** next quick fixpoint iteration ***")
            val filter = InfluencedBySymbolFilter(rwSyms)
            reduceUntilFixpoint(current, Quick(filter), iter, fixpoints + 1)
          }

        case (None, iter, _) =>
          run match {
            case Full | _: QuickRemaining =>
              // all reducers have been applied but
              // no reduction was possible, so fixed point reached
              program
            case Quick(f)                 =>
              // all reducers have been applied without success but
              // we had a quick run
              // need to run the omitted reducers as well
              println("*** next remaining fixpoint iteration (no success) ***")
              reduceUntilFixpoint(program, QuickRemaining(f), iter, fixpoints + 1)
          }
      }
    }

    val reduced = reduceUntilFixpoint(program, run = Run.Full)
    if (config.simplify) {
      ProgramSimplifier(reduced)
    } else {
      reduced
    }

  }

}
