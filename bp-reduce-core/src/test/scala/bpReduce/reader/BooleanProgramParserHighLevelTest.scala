package bpReduce.reader

import bpReduce.{Resources, BaseSpecification}
import bpReduce.ast._
import bpReduce.ast.VariableHolder
import bpReduce.ast.Function
import bpReduce.ast.Program
import bpReduce.ast.Expr._
import bpReduce.ast.Stmt._
import org.springframework.core.io.Resource

class BooleanProgramParserHighLevelTest extends BaseSpecification {
  "test" in {
    val content = Resources.loadFileOrUrl("trace_WP_bug2/main.bp")
    val program = new BooleanProgramParser().parse(content)
    ok
  }
}