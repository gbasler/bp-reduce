package bpReduce
package reduction

import bpReduce.ast._
import bpReduce.ast.Function
import bpReduce.ast.LabelledStmt
import bpReduce.ast.Program
import scala.collection.immutable.Nil
import scala.annotation.tailrec
import bpReduce.ast.Expr.Var
import bpReduce.transformations.VariableCollector
import scala.annotation.tailrec

/**
 * @param original
 * @param reduced
 * @param unreduced `head` is the statement we are currently looking at
 */
final case class PartitionedFunction(original: Function,
                                     reduced: List[LabelledStmt],
                                     unreduced: List[LabelledStmt]) {

  def function(current: Stmt) = {
    unreduced match {
      case Nil          =>
        sys.error("Can't replace a statement in an empty function!")
      case head :: tail =>
        original.copy(stmts = (reduced :+ head.copy(stmt = current)) ++ unreduced)
    }
  }

  def function = original.copy(stmts = reduced ++ unreduced)
}

object PartitionedFunction {
  def apply(function: Function) = {
    new PartitionedFunction(original = function, reduced = Nil, unreduced = function.stmts)
  }
}

final case class ComposedProgramReducer(reducerFactory: StmtReducerFactory,
                                        reducer: StmtReducer,
                                        program: Program,
                                        reduced: List[Function],
                                        unreduced: List[Function],
                                        inProgress: PartitionedFunction,
                                        filter: Option[Set[Sym]]) extends ProgramReducer {

  import ComposedProgramReducer._

  override def rwSyms: Set[Sym] = {
    VariableCollector(reducer.from)
  }

  def current: Program = {
    val stmts = inProgress.unreduced match {
      case Nil          => Nil // TODO: check this case
      case head :: tail =>
        // replace `head` with current reduction
        inProgress.reduced ++ (head.copy(stmt = reducer.to) :: Nil) ++ tail
    }
    val function = inProgress.original.copy(stmts = stmts)
    val functions = (reduced :+ function) ++ unreduced
    program.copy(functions = functions)
  }

  /**
   * Reduces current statement if possible. Keeps last reduction.
   */
  def reduce: Option[ComposedProgramReducer] = {
    reducer.reduce.map {
      stmtReducer =>
      // current statement can be reduced further
        val updatedInProgress = inProgress.unreduced match {
          case Nil          =>
            sys.error("Can't replace a statement in an empty function!")
          case head :: tail =>
            val last = reducer.to
            inProgress.copy(unreduced = head.copy(stmt = last) :: tail)
        }
        copy(reducer = stmtReducer, inProgress = updatedInProgress)
    }.orElse {
      // search for next statement to reduce
      val updatedInProgress = inProgress.unreduced match {
        case Nil          =>
          sys.error("Can't replace a statement in an empty function!")
        case head :: tail =>
          val last = reducer.to
          inProgress.copy(reduced = inProgress.reduced :+ head.copy(stmt = last), unreduced = tail)
      }
      apply(reducerFactory, program, reduced, unreduced, Some(updatedInProgress), filter)
    }
  }

  /**
   * Problem: what shall be advanced? The current reducer _or_ we move to the next location
   * for reduction?
   * Answer: We can just check if the reducer can be advanced, if not, we simply check
   * for the next reduction possibility.
   */
  def advance: Option[ComposedProgramReducer] = {
    reducer.advance.map {
      stmtReducer =>
      // current statement can be reduced further
        copy(reducer = stmtReducer)
    }.orElse {
      // if the reducer can't be advanced, we have to check for next location to reduce
      inProgress.unreduced match {
        case Nil          =>
          // already looked at last statement
          None
        case head :: tail =>
          val updatedInProgress = inProgress.copy(reduced = inProgress.reduced :+ head, unreduced = tail)
          apply(reducerFactory, program, reduced, unreduced, Some(updatedInProgress), filter)
      }
    }
  }

}

object ComposedProgramReducer {

  def apply(reducerFactory: StmtReducerFactory,
            program: Program,
            filter: Option[Set[Sym]]): Option[ComposedProgramReducer] = {

    program.functions match {
      case Nil       =>
        None
      case functions =>
        apply(reducerFactory, program, Nil, functions, None, filter)
    }
  }

  def apply(reducerFactory: StmtReducerFactory,
            program: Program,
            reduced: List[Function],
            unreduced: List[Function],
            inProgress: Option[PartitionedFunction],
            filter: Option[Set[Sym]]): Option[ComposedProgramReducer] = {

    /**
     * @param reduced
     * @param unreduced
     * @param inProgress Contains the latest statement that _has already_ been reduced
     *                   so it must be replace with a new one.
     * @return
     */
    @tailrec
    def findNextStmt(reduced: List[Function],
                     unreduced: List[Function],
                     inProgress: PartitionedFunction): Option[ComposedProgramReducer] = {

      inProgress.unreduced match {
        case Nil          =>
          // look at next function
          unreduced match {
            case Nil      =>
              // last function, no reduction possible
              None
            case hd :: tl =>
              // look at next function
              findNextStmt(reduced :+ inProgress.function, tl.tail, PartitionedFunction(tl.head))
          }
        case head :: tail =>

          // skip stmts that do not pass filter
          val passedFilter = filter.map {
            filter => (VariableCollector(head.stmt) union filter).isEmpty
          }.getOrElse(true)

          if (passedFilter) {
            // stay in current function: search for next stmt to reduce...
            val reducer = reducerFactory(head.stmt)
            reducer match {
              case Some(reducer) =>
                // reduction possible on that statement
                Some(apply(reducerFactory, reducer, program, reduced, unreduced, inProgress, filter = filter))
              case None          =>
                // reducer can't reduce that statement, take next one
                findNextStmt(reduced, unreduced,
                  inProgress.copy(reduced = inProgress.reduced :+ head, unreduced = tail))
            }
          } else {
            findNextStmt(reduced, unreduced,
              inProgress.copy(reduced = inProgress.reduced :+ head, unreduced = tail))
          }
      }
    }

    inProgress.fold(
      // bootstrap: start at first function
      // we still need to search through it's statements since
      // e.g., the first one could be non-reducible
      program.functions match {
        case Nil          =>
          None
        case head :: tail =>
          val inProgress = PartitionedFunction(head)
          findNextStmt(Nil, tail, inProgress)
      })(inProgress => findNextStmt(reduced, unreduced, inProgress))
  }
}
