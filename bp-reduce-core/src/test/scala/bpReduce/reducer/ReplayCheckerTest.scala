package bpReduce
package reducer

import bpReduce.reduction.Reducers
import bpReduce.transformations.ProgramSimplifier

class ReplayCheckerTest extends BaseSpecification {
  "replay one run" in {
    val outputChecker = ErrorOutputChecker.Default
    val replays = Resources.getFileForUrlOrFile("replays/2014-03-03-01-29-40")
    val checker = ReplayChecker(outputChecker, replays.toPath, verbose = true)
    val cfg = ReducerConfig(reducers = Reducers.All, checker = checker, simplify = true)
    val reducer = new Reducer(cfg)
    val program = Resources.loadProgramFromFileOrUrl("trace_WP_bug2/main.bp")
    val simplified = ProgramSimplifier(program)
    val reduced = reducer(simplified)
    val expected = Resources.loadProgramFromFileOrUrl("trace_WP_bug2/reduced.bp")
    reduced must beSameProgram(expected)
  }
}
