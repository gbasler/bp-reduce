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
}
