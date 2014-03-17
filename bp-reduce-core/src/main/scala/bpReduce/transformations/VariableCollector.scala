package bpReduce
package transformations

import bpReduce.ast._
import scala.collection.mutable.ListBuffer
import bpReduce.ast.LabelledStmt
import bpReduce.ast.Expr.Var

object VariableCollector {
  /**
   * @return All variables that are read / written by a statement.
   */
  def apply(stmt: Stmt): Set[Sym] = {
    val results = new ListBuffer[Sym]

    new StmtTraverserForExpr({
      case v: Var => results += v.sym
    }).traverse(stmt)

    results.toSet
  }
}
