package bpReduce.matcher

import bpReduce.ast.Program

trait ProgramMatchers {
  outer =>

  def beSameProgram(program: Program) = new ProgramMatcher(program)
}
