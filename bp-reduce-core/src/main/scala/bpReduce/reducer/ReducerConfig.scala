package bpReduce
package reducer

import bpReduce.reduction.ProgramReducerFacory

/**
 *
 * @param reducers
 * @param checker
 */
final case class ReducerConfig(reducers: List[ProgramReducerFacory],
                               checker: Checker,
                               simplify: Boolean)