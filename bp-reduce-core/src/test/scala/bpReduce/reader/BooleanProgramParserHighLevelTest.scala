package bpReduce
package reader

import bpReduce.{Resources, BaseSpecification}
import bpReduce.writer.Formatter

class BooleanProgramParserHighLevelTest extends BaseSpecification {
  "eat your own dogfood" in {
    val content = Resources.loadFileOrUrl("trace_WP_bug2/main.bp")
    val program = new BooleanProgramParser().parse(content)
    val prettyContent = Formatter.format(program)
    println(prettyContent)
    val reparsed = new BooleanProgramParser().parse(prettyContent)

    ok
  }
}