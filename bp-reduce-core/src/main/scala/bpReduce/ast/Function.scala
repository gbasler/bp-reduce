package bpReduce.ast

case class Function(name: String,
                    locals: VariableHolder,
                    args: List[String],
                    returns: Int,
                    stmts: List[Stmt])