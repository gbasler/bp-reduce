package bpReduce
package reduction

import bpReduce.ast.Stmt

trait StmtFilter {
  def filter(stmt: Stmt): Boolean
}

object StmtFilter {
  val Empty = new StmtFilter {
    def filter(stmt: Stmt): Boolean = true
  }

  def invert(filter:StmtFilter) = new StmtFilter {
    def filter(stmt: Stmt): Boolean = !filter(stmt)
  }
}
