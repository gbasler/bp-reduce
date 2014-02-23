package bpReduce
package reducer

import bpReduce.reduction.ProgramReducerFacory

/**
 *
 * @param reducers
 * @param checker
 * @param cache note that the cache contains state!!!
 */
final case class ReducerConfig(reducers: List[ProgramReducerFacory],
                               checker: Checker,
                               cache: ProgramCache)