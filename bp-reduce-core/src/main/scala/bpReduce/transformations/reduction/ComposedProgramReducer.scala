package bpReduce
package transformations
package reduction

import bpReduce.ast._
import bpReduce.ast.Function
import bpReduce.ast.LabelledStmt
import bpReduce.ast.Program

case class ComposedProgramReducer(stmtReducer: StmtReducer,
                                  program: Program) extends ProgramReducer {

  val reduced: Seq[Function] = Seq()
  val unreduced: Seq[Function] = program.functions

  final case class PartitionedFunction(name: String,
                                       locals: VariableHolder,
                                       args: Seq[String],
                                       returns: Int,
                                       reduced: Seq[LabelledStmt],
                                       unreduced: Seq[LabelledStmt])

  object PartitionedFunction {
    def apply(function: Function) = {
      import function._
      new PartitionedFunction(name, locals, args, returns)
    }
  }

  val inProgress = PartitionedFunction()


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
