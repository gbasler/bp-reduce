package bpReduce
package ast

import bpReduce.ast.Stmt._

class StmtTransformer {

  def transform(stmt: LabelledStmt): LabelledStmt = stmt.stmt match {
    case If(condition, pos, neg) =>
      stmt.copy(stmt = If(condition, transformTrees(pos), transformTrees(neg)))
    case _                       => stmt
  }

  /** Traverses a list of stmts. */
  def transformTrees(trees: List[LabelledStmt]): List[LabelledStmt] = {
    trees map transform
  }
}
