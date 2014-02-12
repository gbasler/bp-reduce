package bpReduce
package transformations
package reduction

import bpReduce.ast._
import bpReduce.ast.Function
import bpReduce.ast.LabelledStmt
import bpReduce.ast.Program
import scala.collection.immutable.Nil
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
                                        inProgress: PartitionedFunction) extends ProgramReducer {

  import ComposedProgramReducer._

  override def current: Option[Program] = {
    reducer.current.map {
      stmt: Stmt =>
        val stmts = inProgress.unreduced match {
          case Nil          => Nil // TODO: check this case
          case head :: tail =>
            inProgress.reduced ++ (head.copy(stmt = stmt) :: Nil) ++ tail
        }
        val function = inProgress.original.copy(stmts = stmts)
        val functions = (reduced :+ function) ++ unreduced
        program.copy(functions = functions)
    }
  }

  override def reduce: Option[ComposedProgramReducer] = {
    reducer.reduce match {
      case Some(stmtReducer) =>
        // current statement can be reduced further
        Some(copy(reducer = stmtReducer))

      case None // advance recuder!!!
    }
  }

  override def advance: Option[ComposedProgramReducer] = {
    case None              =>
      // look for next statement to reduce
      inProgress.unreduced match {
        case Nil          =>
          // already looked at last statement
          None
        case head :: tail =>
          val updatedInProgress = inProgress.copy(reduced = inProgress.unreduced :+ head, unreduced = tail)
          apply(reducerFactory, program, reduced, unreduced, Some(updatedInProgress))
      }

  }
}

object ComposedProgramReducer {

  def apply(reducerFactory: StmtReducerFactory,
            program: Program): Option[ComposedProgramReducer] = {

    program.functions match {
      case Nil       =>
        None
      case functions =>
        apply(reducerFactory, program, Nil, functions, None)
    }
  }

  def apply(reducerFactory: StmtReducerFactory,
            program: Program,
            reduced: List[Function],
            unreduced: List[Function],
            inProgress: Option[PartitionedFunction]): Option[ComposedProgramReducer] = {

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
        case Nil      =>
          // look at next function
          unreduced match {
            case Nil      =>
              // last function, no reduction possible
              None
            case hd :: tl =>
              // look at next function
              findNextStmt(reduced :+ inProgress.function, tl.tail, PartitionedFunction(tl.head))
          }
        case hd :: tl =>
          // stay in current function: search for next stmt to reduce...
          val reducer = reducerFactory.create(hd.stmt)
          if (reducer.current.isDefined) {
            // reduction possible on that statement
            Some(ComposedProgramReducer(reducerFactory, reducer, program, reduced, unreduced, inProgress))
          } else {
            // reducer can't reduce that statement, take next one
            findNextStmt(reduced, unreduced,
              inProgress.copy(reduced = inProgress.reduced :+ hd, unreduced = tl))
          }
      }
    }

    inProgress match {
      case None             =>
        // bootstrap: start at first function
        // we still need to search through it's statements since
        // e.g., the first one could be non-reducible
        program.functions match {
          case Nil          =>
            None
          case head :: tail =>
            val inProgress = PartitionedFunction(head)
            findNextStmt(Nil, tail, inProgress)
        }
      case Some(inProgress) =>
        findNextStmt(reduced, unreduced, inProgress)
    }
  }
}
