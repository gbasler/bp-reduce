package bpReduce
package reducer

import bpReduce.reduction.ProgramReducerFactory

final case class ReducerConfig(reducers: List[ProgramReducerFactory],
                               checker: Checker,
                               simplify: Boolean,
                               smartAcceleration: Boolean)