package bpReduce
package reduction

import bpReduce.ast.{Sym, Stmt}
import bpReduce.transformations.VariableCollector

trait StmtFilter {
  def filter(stmt: Stmt): Boolean

  def negate: StmtFilter
}

final case class InfluencedBySymbolFilter(rwSyms: Set[Sym]) extends StmtFilter {
  def filter(stmt: Stmt): Boolean = {
    val commonSyms = VariableCollector(stmt) intersect rwSyms
    commonSyms.nonEmpty
  }

  def negate = NotInfluencedBySymbolFilter(rwSyms)
}

final case class NotInfluencedBySymbolFilter(rwSyms: Set[Sym]) extends StmtFilter {
  def filter(stmt: Stmt): Boolean = {
    val commonSyms = VariableCollector(stmt) intersect rwSyms
    commonSyms.isEmpty
  }

  def negate = InfluencedBySymbolFilter(rwSyms)
}


object StmtFilter {
  val Empty = new StmtFilter {
    def filter(stmt: Stmt): Boolean = true

    def negate = sys.error("Pointless filter.")
  }
}
