package bpReduce
package ast

final case class LabelledStmt(stmt: Stmt, labels: Seq[String])


