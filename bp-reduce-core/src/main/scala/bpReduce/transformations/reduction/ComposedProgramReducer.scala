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
                                     reduced: Seq[LabelledStmt],
                                     unreduced: Seq[LabelledStmt]) {
  def function = original.copy(stmts = reduced ++ unreduced)
}

object PartitionedFunction {
  def apply(function: Function) = {
    new PartitionedFunction(original = function, reduced = Seq(), unreduced = function.stmts)
  }
}

final case class ComposedProgramReducer(stmtReducer: StmtReducer,
                                        program: Program,
                                        reduced: Seq[Function],
                                        unreduced: Seq[Function],
                                        inProgress: PartitionedFunction) extends ProgramReducer {

  def next = {

    @tailrec
    def findNextStmt(reduced: List[Function],
                     unreduced: List[Function],
                     inProgress: PartitionedFunction): Option[ComposedProgramReducer] = {
      val newInProgress = if (inProgress.unreduced.isEmpty) {
        // next function
        unreduced match {
          case Nil      =>
            // last function, no reduction possible
            None
          case hd :: tl =>
            // look at next function
            findNextStmt(reduced :+ inProgress.function, tl.tail, PartitionedFunction(tl.head))
        }
      }

    }


  }

  // TODO: this will not work for object Stmts! need to find another way...
  // can not use eq since skip is case object not class!
  val candidates = program.filter(stmtReducer.isDefinedAt)

  val reductions = Seq[Stmt => Program]()

  val r = (s: Stmt) => {

    val copiedSoFar =

      for {
        f <- program.functions
      } yield {
        val stmts: Seq[LabelledStmt] = for {
          l@LabelledStmt(stmt, labels) <- f.stmts
        } yield {
          if (stmtReducer.isDefinedAt(stmt)) {
            LabelledStmt(s, labels)
          } else {
            l
          }
        }

        f.copy(stmts = stmts)
      }
  }


  override def current = ???

  override def reduce = ???

  override def advance = ???
}

object ComposedProgramReducer {
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