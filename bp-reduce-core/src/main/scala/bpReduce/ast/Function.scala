package bpReduce
package ast

import scala.collection.mutable.ListBuffer

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

    for {
      LabelledStmt(stmt, labels) <- stmts
    } {
      if (pf.isDefinedAt(stmt)) results += pf(stmt)
    }

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

}