package bpReduce
package transformations
package reduction

import bpReduce.ast.Stmt

trait StmtReducerFactory {
  /**
   * @return new reducer, ready to perform first transformation.
   */
  def create(stmt: Stmt): StmtReducer
}
