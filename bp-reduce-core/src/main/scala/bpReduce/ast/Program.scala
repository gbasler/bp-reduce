package bpReduce.ast

case class Program(globals: VariableHolder, functions: Seq[Function])