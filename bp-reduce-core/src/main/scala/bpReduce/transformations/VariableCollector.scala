package bpReduce
package transformations

import bpReduce.ast.Expr.Var
import bpReduce.ast.Stmt.Assign
import bpReduce.ast._

import scala.collection.mutable.ListBuffer

object VariableCollector {
  /**
   * @return All variables that are read / written by a statement.
   */
  def apply(stmt: Stmt): Set[Sym] = {
    val results = new ListBuffer[Sym]

    new StmtTraverserForExpr({
      case v: Var => results += v.sym
    }).traverse(stmt)

    stmt match {
      case Assign(assigns, constrain) =>
        results ++= assigns.map(_._1.sym)
      case _ =>
    }

    results.toSet
  }
}
