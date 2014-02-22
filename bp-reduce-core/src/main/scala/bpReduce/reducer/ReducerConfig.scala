package bpReduce
package reducer

import bpReduce.reduction.ProgramReducerFacory

final case class ReducerConfig(reducers: List[ProgramReducerFacory],
                               checker: Checker)