package bpReduce
package ast

final case class Program(globals: VariableHolder, functions: Seq[Function])