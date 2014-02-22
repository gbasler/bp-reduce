package bpReduce
package reduction

import bpReduce.ast.Stmt

trait StmtReducerFactory {
  /**
   * @return new reducer, ready to perform first transformation.
   */
  def apply(stmt: Stmt): Option[StmtReducer]
}
