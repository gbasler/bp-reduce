package bpReduce
package reducer

import bpReduce.ast.{Sym, Program}
import bpReduce.transformations.reduction.{ReduceExpr, Reducers}
import bpReduce.reader.BooleanProgramParser
import bpReduce.ast.Stmt.{Assume, Assign}
import bpReduce.ast.Expr.{False, Var}
import bpReduce.ast.StateIdentifier.Current
import bpReduce.ast.MixedIdentifier.NonMixed
import bpReduce.transformations.ExpressionSimplifier

class ReducerHighLevelTest extends BaseSpecification {
  implicit def fromText(program: String) = {
    new BooleanProgramParser().parse(program)
  }

  "complex reducer algorithm" should {
    "reduce with smart checker" in {

      // checker that accepts any program that sets `g`
      val smartChecker = new Checker {
        def apply(program: Program): CheckerResult = {
          val ok = program.exists {
            case Assign(vars, _) =>
              vars.exists {
                case (Var(Sym("g"), Current, NonMixed), _) => true
                case _                                     => false
              }
            case _               =>
              false
          }
          if (ok) {
            CheckerResult.Accept
          } else {
            CheckerResult.Reject
          }

        }
      }

      import Reducers._
      val config = ReducerConfig(List(ReplaceWithSkip, ReduceAssigns), smartChecker)

      val program: Program =
        """|decl g;
          |void main()
          |begin
          |decl l;
          |decl a;
          |a := T;
          |a := !a;
          |atomic_begin;
          |g, l := l, g;
          |atomic_end;
          |return;
          |end
          |
        """.stripMargin

      val reduced: Program =
        """|decl g;
          |void main()
          |begin
          |decl l;
          |decl a;
          |skip;
          |skip;
          |skip;
          |g := l;
          |skip;
          |skip;
          |end
          |
        """.stripMargin

      Reducer(config)(program) must beSameProgram(reduced)
    }

    "reduce expression with smart checker" in {

      // checker that accepts any program that sets `g`
      val smartChecker = new Checker {
        def apply(program: Program): CheckerResult = {
          val ok = program.exists {
            case Assume(e) =>
              // rule out false case since T / F would both be possible (makes test deterministic)
              ExpressionSimplifier(e) != False
            case _         => false
          }
          if (ok) {
            CheckerResult.Accept
          } else {
            CheckerResult.Reject
          }

        }
      }

      import Reducers._
      val config = ReducerConfig(List(ReplaceWithSkip, ReduceAssigns, ReduceExpr), smartChecker)

      val program: Program =
        """|decl g;
          |void main()
          |begin
          |decl l;
          |decl a;
          |a := T;
          |a := !a;
          |atomic_begin;
          |assume(l = g);
          |atomic_end;
          |return;
          |end
          |
        """.stripMargin

      val reduced: Program =
        """|decl g;
          |void main()
          |begin
          |decl l;
          |decl a;
          |skip;
          |skip;
          |skip;
          |assume(T);
          |skip;
          |skip;
          |end
          |
        """.stripMargin

      Reducer(config)(program) must beSameProgram(reduced)
    }
  }
}
