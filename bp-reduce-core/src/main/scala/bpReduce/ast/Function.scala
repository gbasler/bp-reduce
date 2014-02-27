package bpReduce
package ast

import scala.collection.mutable.ListBuffer
import bpReduce.ast.Stmt._

final case class Function(name: String,
                          locals: VariableHolder = VariableHolder(),
                          args: Seq[String] = Seq(),
                          returns: Int = 0,
                          stmts: List[LabelledStmt] = Nil) {

  /**
   * @param pf Applied to each expr on which the function is defined and collect the results.
   */
  def collect[T](pf: PartialFunction[Stmt, T]): List[T] = {
    val results = new ListBuffer[T]

    def collectStmts(stmts: List[LabelledStmt]) {
      for {
        LabelledStmt(stmt, labels) <- stmts
      } {
        if (pf.isDefinedAt(stmt)) results += pf(stmt)
        stmt match {
          case If(_, pos, neg) =>
            collectStmts(pos)
            collectStmts(neg)
          case _               =>
        }
      }
    }

    collectStmts(stmts)

    results.toList
  }

  def filter(predicate: Stmt => Boolean): List[Stmt] = {
    val results = new ListBuffer[Stmt]

    for {
      LabelledStmt(stmt, labels) <- stmts
    } {
      if (predicate(stmt)) results += stmt
    }

    results.toList
  }

  def contains(predicate: Stmt => Boolean): Boolean = {
    for {
      LabelledStmt(stmt, labels) <- stmts
    } {
      if (predicate(stmt)) return true
    }

    false
  }

  /**
   * @param pf Applied to each stmt on which the function is defined and
   *           returns a new function with the transformed statements.
   */
  def transform(pf: PartialFunction[LabelledStmt, LabelledStmt]): Function = {

    val transformer = new StmtTransformer {

      override def transform(stmt: LabelledStmt): LabelledStmt = {
        if (pf.isDefinedAt(stmt)) {
          pf(stmt)
        } else {
          super.transform(stmt)
        }
      }

    }


    copy(stmts = transformer.transformTrees(stmts))
  }

}