package bpReduce

import org.specs2.execute.PendingUntilFixed
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import bpReduce.matcher.ProgramMatchers

trait BaseSpecification extends Specification with ScalaCheck with PendingUntilFixed with ProgramMatchers
