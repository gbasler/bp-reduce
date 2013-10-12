package bpReduce.ast

case class Function(name: String,
                    locals: VariableHolder,
                    args: Seq[String],
                    returns: Int,
                    stmts: Seq[Stmt])