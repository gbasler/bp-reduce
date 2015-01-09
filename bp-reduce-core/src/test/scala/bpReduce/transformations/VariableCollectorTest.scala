package bpReduce.transformations

import bpReduce.ast.{Sym, Program}
import bpReduce.reader.BooleanProgramParser
import org.specs2.mutable.Specification

class VariableCollectorTest extends Specification {
  implicit def fromText(program: String) = {
    new BooleanProgramParser().parse(program)
  }

  "assign" in {
    val program: Program =
      """|decl b0_s_le_2;
        |decl b1;
        |decl b2;
        |void main() begin
        |	decl b3_l_eq_s;
        |	decl b4_0_eq_l;
        |	decl b5_1_eq_l;
        |PC20:	b3_l_eq_s := T constrain !b4_0_eq_l | b4_0_eq_l$;
        |end
      """.stripMargin

    val variables = program.functions.head.collect[Set[Sym]] {
      case stmt => VariableCollector(stmt)
    }.flatten.toSet

    variables === Set(Sym("b4_0_eq_l"), Sym("b3_l_eq_s"))
  }
}
