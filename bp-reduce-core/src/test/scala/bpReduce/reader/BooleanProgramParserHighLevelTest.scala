package bpReduce
package reader

import bpReduce.{Resources, BaseSpecification}
import bpReduce.writer.Formatter

class BooleanProgramParserHighLevelTest extends BaseSpecification {
  "test" in {
    val content = Resources.loadFileOrUrl("trace_WP_bug2/main.bp")
    val program = new BooleanProgramParser().parse(content)
    println(Formatter.format(program))
    ok
  }
}