package bpReduce.writer

import bpReduce.reader.BooleanProgramParser
import bpReduce.{Resources, BaseSpecification}

class FormatterTest extends BaseSpecification {
  "eat your own dogfood" in {
    val content = Resources.loadFileOrUrl("trace_WP_bug2/main.bp")
    val program = new BooleanProgramParser().parse(content)
    val prettyContent = Formatter.format(program)
    val reparsed = new BooleanProgramParser().parse(prettyContent)
    program must be_==(reparsed)
  }
}