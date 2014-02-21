package bpReduce
package transformations
package reduction

import bpReduce.reader.BooleanProgramParser
import bpReduce.ast.Stmt
import bpReduce.ast.Stmt.{Assume, Assign}

object ReductionChain {
  implicit def stmtFromString(str: String): Stmt = {
    new BooleanProgramParser().parseStmt(str)
  }

  implicit def assignFromString(str: String): Assign = {
    new BooleanProgramParser().parseAssign(str)
  }

  implicit def assumeFromString(str: String): Assume = {
    new BooleanProgramParser().parseAssume(str)
  }

  implicit class UnparsedStmtWrapper(val stmt: String) extends AnyVal {
    def ->(s: String): Reduction = {
      val parser = new BooleanProgramParser()
      Reduction(parser.parseStmt(stmt), parser.parseStmt(s))
    }
  }

  implicit class StmtWrapper(val stmt: Stmt) extends AnyVal {
    def reducesTo(s: Stmt): Reduction = Reduction(stmt, s)
  }

  final case class Reduction(from: Stmt, to: Stmt)

  def buildTree(reductions: Seq[Reduction]): Map[Stmt, Set[Stmt]] = {
    reductions.groupBy(_.from).map {
      case (stmt, reductions) =>
        stmt -> reductions.map(_.to).toSet
    }
  }

}