package bpReduce
package ast

final case class Function(name: String,
                          locals: VariableHolder,
                          args: Seq[String],
                          returns: Int,
                          stmts: Seq[LabelledStmt])