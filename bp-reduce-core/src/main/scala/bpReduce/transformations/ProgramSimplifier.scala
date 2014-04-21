package bpReduce
package transformations

import bpReduce.ast._
import bpReduce.ast.Stmt._
import bpReduce.ast.Expr.True
import bpReduce.ast.Stmt.Call
import bpReduce.ast.Stmt.Assume
import bpReduce.ast.Function
import bpReduce.ast.Stmt.Assign
import bpReduce.ast.Stmt.Assert
import bpReduce.ast.Stmt.If
import bpReduce.ast.Stmt.Return
import bpReduce.ast.Program
import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
 * Checking a program is a very expensive operation.
 * Thus in order to arrive as quickly as possible at the most reduced program
 * (preserving the property that causes the bug in the model checker),
 * the calls to the model checker should be minimized.
 *
 * One method to reduce the number of calls is to check only one variant of a class of
 * programs (e.g., assuming that the all either have the property or they all don't have it).
 * A possible way of grouping the programs is to simplify them in such a way that the simplifications do not
 * alter the outcome.
 *
 * <ul>
 * <li>We assume that expressions can be simplified. The reason is that a simplified and non-simplified
 * expression results (for a BDD-based model checker) in exactly the same transition function.</li>
 * <li>`skip` statements do not alter the outcome as well. They only modify the program counter but
 * no variable. Thus the model checker does not alter any BDD's / SAT formulas. The program counter
 * logic is most likely correct since the model checker would most likely fail on many other programs.
 * </li>
 * </ul>
 *
 * These assumptions of course do not need to hold in any case but in practice I've never seen the contrary.
 */
object ProgramSimplifier {

  final case class Block(stmt: List[Stmt], labels: Seq[String])

  final case class FunctionUnderSimplification(blocks: Seq[Block])

  def apply(program: Program): Program = {
    val transformations = Seq(simplifyStmts _, removeNonTargetSkips _, simplifyExprs _)
    val simplify = transformations.reduce(_.compose(_))

    val functions = for {
      function <- program.functions
      simplified = simplify(function)
    } yield simplified
    program.copy(functions = functions)
  }

  def simplifyVariablesAndFunctions(program: Program): Program = {
    val transformations = Seq(simplifyVariables _, removeDeadFunctions _)
    val simplify = transformations.reduce(_.compose(_))

    simplify(program)
  }

  def simplifyVariables(program: Program): Program = {
    val (simplifiedProgam, usedVars) = removeDeadVariables(program)

    // we assume that locals never shadow globals...
    simplifiedProgam.copy(globals = VariableHolder(simplifiedProgam.globals.vars.filter(usedVars.contains)))
  }

  /**
   * Very very conservative algorithm:
   * Removes all skips but the ones that are targets of gotos etc.
   * Most likely a more aggressive algorithm would work too but
   * the conservative algorithm is easier to implement and produces acceptable
   * results.
   *
   * TODO: check how much improvement a more aggressive algorithm would bring
   *
   * @param function
   */
  private def removeNonTargetSkips(function: Function) = {
    // if a skip has a label that is used, it will not be removed
    val usedLabels: Set[String] = TargetCollector(function)
    val stmts = function.stmts.filterNot {
      stmt =>
        stmt.stmt == Skip && stmt.labels.forall(label => !usedLabels.contains(label))
    }
    function.copy(stmts = stmts)
  }

  private def simplifyStmts(function: Function) = {
    function.transformInside {
      case assign@Assign(_, Some(True)) =>
        assign.copy(constrain = None)
    }
  }

  private def simplifyExprs(function: Function) = {
    function.transformInside {
      case Assign(assigns, constrain) =>
        val a = assigns.map {
          case (value, expr) => value -> ExpressionSimplifier(expr)
        }
        val c = constrain.map(ExpressionSimplifier.apply)
        Assign(assigns = a, constrain = c)
      case assume: Assume =>
        assume.copy(ExpressionSimplifier(assume.e))
      case Assert(e) =>
        Assert(ExpressionSimplifier(e))
      case call: Call =>
        val args = call.args.map(ExpressionSimplifier.apply)
        call.copy(args = args)
      case If(condition, pos, neg) =>
        val transformer = new StmtTransformer
        val c = ExpressionSimplifier(condition)
        val p = pos.map(transformer.transform)
        val n = neg.map(transformer.transform)
        If(c, p, n)
      case Return(values) =>
        Return(values.map(ExpressionSimplifier.apply))
    }
  }

  private def removeDeadVariables(program: Program): (Program, Set[Sym]) = {
    val (functions, usedVariables) = {
      for {
        function <- program.functions
      } yield removeDeadVariables(function)
    }.unzip

    program.copy(functions = functions) -> usedVariables.toSet.flatten
  }

  /**
   * Deletes unused variables.
   */
  private def removeDeadVariables(function: Function) = {
    val used: Set[Sym] = {
      function.collect[Set[Sym]] {
        case stmt => VariableCollector(stmt)
      }
    }.toSet.flatten

    function.modifyVars(h => h.copy(vars = h.vars.filter(used.contains))) -> used
  }

  /**
   * Deletes unused functions.
   */
  def removeDeadFunctions(program: Program) = {

    def calleesForFunction(function: Function) = {
      function.collect {
        case Call(name, _, _) => name
      }
    }.toSet

    val calleesForFunctions = program.functions.map(f => f.name -> calleesForFunction(f)).toMap

    @tailrec
    def reachableFunctions(wl: List[String],
                           visited: Set[String] = Set()): Set[String] = wl match {
      case Nil =>
        visited
      case next :: frontier =>
        if (visited.contains(next)) {
          reachableFunctions(frontier, visited)
        } else {
          reachableFunctions(calleesForFunctions(next).toList ::: wl, visited + next)
        }
    }

    val usedFunctions = reachableFunctions("main" :: Nil)
    program.copy(functions = program.functions.filter(f => usedFunctions.contains(f.name)))
  }

}
