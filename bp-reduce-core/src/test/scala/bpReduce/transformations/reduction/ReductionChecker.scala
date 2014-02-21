package bpReduce
package transformations
package reduction

import bpReduce.ast.Stmt
import scala.annotation.tailrec

object ReductionChecker extends BaseSpecification {

  import ReductionChain._

  def apply(reducer: StmtReducer,
            origin: Stmt,
            reductions: Seq[Reduction]) = {
    // we build a tree for convenience
    val tree: Map[Stmt, Set[Stmt]] = buildTree(reductions)

    val leftOver = checkReductionChain(tree, reducer :: Nil, reductions.toSet)
    leftOver must beEmpty
  }

  /**
   * Goes down all reducer paths from a given root.
   */
  @tailrec
  private def checkReductionChain(tree: Map[Stmt, Set[Stmt]],
                                  wl: List[StmtReducer],
                                  unused: Set[Reduction]): Set[Reduction] = {

    def checkReferenceReduction(reducer: StmtReducer,
                                unused: Set[Reduction]) = {
      val from = reducer.from
      val to = reducer.to
      val refTos = tree.getOrElse(from, sys.error(s"there should be no reduction possible from $from to $to"))
      refTos aka s"""$from -> ${refTos.mkString(",")}""" must contain(to)
      unused - Reduction(from, to)
    }

    wl match {
      case Nil             =>
        unused
      case reducer :: tail =>

        // see what reducer offers...
        val unusedWithoutCurrent = checkReferenceReduction(reducer, unused)

        val (updatedWl, unusedWithoutReduced) = reducer.reduce match {
          case Some(reduced) =>
            (tail :+ reduced) -> checkReferenceReduction(reduced, unusedWithoutCurrent)
          case None          =>
            tail -> unusedWithoutCurrent
        }

        val nextWl = reducer.advance match {
          case Some(advancedReducer) =>
            // reduction possible... check
            updatedWl :+ advancedReducer
          case None                  =>
            updatedWl
        }

        checkReductionChain(tree, nextWl, unusedWithoutReduced)
    }
  }

}
