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
                                     reduced: Seq[LabelledStmt],
                                     unreduced: List[LabelledStmt]) {
  def function = original.copy(stmts = reduced.toList ++ unreduced)
}

object PartitionedFunction {
  def apply(function: Function) = {
    new PartitionedFunction(original = function, reduced = Nil, unreduced = function.stmts)
  }
}

final case class ComposedProgramReducer(reducerFactory: StmtReducerFactory,
                                        reducer: StmtReducer,
                                        program: Program,
                                        reduced: Seq[Function],
                                        unreduced: Seq[Function],
                                        inProgress: PartitionedFunction) extends ProgramReducer {

  override def current: Option[Program] = {
    reducer.current.map {
      stmt: Stmt =>
        val stmts = inProgress.unreduced match {
          case Nil          => Nil // TODO: check this case
          case head :: tail =>
            (inProgress.reduced :+ head.copy(stmt = stmt)).toList ::: tail
        }
        val function = inProgress.original.copy(stmts = stmts)
        val functions = (reduced :+ function) ++ unreduced
        program.copy(functions = functions)
    }
  }

  override def reduce = None

  override def advance = None
}

object ComposedProgramReducer {

  def apply(reducerFactory: StmtReducerFactory,
            program: Program): Option[ComposedProgramReducer] = {

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

    if (program.functions.isEmpty) {
      None
    } else {
      val unreduced: Seq[Function] = program.functions.tail

      val inProgress = PartitionedFunction(program.functions.head)

      findNextStmt(Nil, unreduced.toList, inProgress)
    }
  }
}
