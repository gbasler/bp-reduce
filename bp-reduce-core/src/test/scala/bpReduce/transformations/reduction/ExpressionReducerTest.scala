package bpReduce
package transformations
package reduction


import bpReduce.BaseSpecification
import bpReduce.ast.Expr._
import bpReduce.ast.Sym
import bpReduce.ast.Expr.Var

class ExpressionReducerTest extends BaseSpecification {

  "reduce expressions" should {

    "no reduction" in {
      ExpressionReducer(True) must be_==(Set())
      ExpressionReducer(False) must be_==(Set())
      ExpressionReducer(Nondet) must be_==(Set())
    }

    "one reduction" in {
      val a = Var(Sym("a"))
      val b = Var(Sym("b"))

      ExpressionReducer(And(a, b)) must be_==(Set(a, b, True, False))
    }

    "two reductions" in {
      val a = Var(Sym("a"))
      val b = Var(Sym("b"))
      val c = Var(Sym("c"))

      ExpressionReducer(And(a, Or(b, c))) must be_==(Set(And(a, b), And(a, c), Or(b, c), a, b, c, True, False))
    }
  }

}
