package bpReduce
package transformations
package reduction

import bpReduce.ast._
import bpReduce.ast.Function
import bpReduce.ast.LabelledStmt
import bpReduce.ast.Program
import scala.collection.immutable.Nil
import scala.annotation.tailrec

final case class PartitionedFunction(original: Function,
                                     reduced: List[LabelledStmt],
                                     unreduced: List[LabelledStmt]) {
  def function = original.copy(stmts = reduced ++ unreduced)
}

object PartitionedFunction {
  def apply(function: Function) = {
    new PartitionedFunction(original = function, reduced = Nil, unreduced = function.stmts)
  }
}

final case class ComposedProgramReducer(reducerFactory: StmtReducerFactory,
                                        current: StmtReducer,
                                        program: Program,
                                        reduced: Seq[Function],
                                        unreduced: Seq[Function],
                                        inProgress: PartitionedFunction) extends ProgramReducer {

  override def current = ???

  override def reduce = ???

  override def advance = ???
}

object ComposedProgramReducer {

  @tailrec
  def findNextStmt(reducerFactory: StmtReducerFactory,
                   reduced: List[Function],
                   unreduced: List[Function],
                   inProgress: PartitionedFunction): Option[ComposedProgramReducer] = {

    inProgress.unreduced match {
      case Nil      =>
        // next function
        unreduced match {
          case Nil      =>
            // last function, no reduction possible
            None
          case hd :: tl =>
            // look at next function
            findNextStmt(reducerFactory, reduced :+ inProgress.function, tl.tail, PartitionedFunction(tl.head))
        }
      case hd :: tl =>
        // search for next stmt in current function
        val reducer = reducerFactory.create(hd.stmt)
        if (reducer.current.isDefined) {
          Some(ComposedProgramReducer(reducerFactory, reducer))
        } else {
          findNextStmt(reduced,)
        }
    }

  }


  def apply(stmtReducer: StmtReducer, program: Program): Option[ComposedProgramReducer] = {

    if (program.functions.isEmpty) {
      None
    } else {
      val reduced: Seq[Function] = Seq()
      val unreduced: Seq[Function] = program.functions.tail

      val inProgress = PartitionedFunction(program.functions.head)

      Some(new ComposedProgramReducer(stmtReducer, program, reduced, unreduced, inProgress))
    }
  }
}