package bpReduce
package transformations
package reduction

import bpReduce.ast.{LabelledStmt, Function, Stmt, Program}

case class ComposedProgramReducer(stmtReducer: StmtReducer,
                                  program: Program) extends ProgramReducer {

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
