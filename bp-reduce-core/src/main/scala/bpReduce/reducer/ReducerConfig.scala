package bpReduce
package reducer

import bpReduce.transformations.reduction.ProgramReducerFacory

final case class ReducerConfig(reducers: List[ProgramReducerFacory],
                               checker: Checker)