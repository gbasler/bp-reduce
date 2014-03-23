package bpReduce
package reducer

import bpReduce.reduction.ProgramReducerFactory

/**
 *
 * @param reducers
 * @param checker
 */
final case class ReducerConfig(reducers: List[ProgramReducerFactory],
                               checker: Checker,
                               simplify: Boolean)