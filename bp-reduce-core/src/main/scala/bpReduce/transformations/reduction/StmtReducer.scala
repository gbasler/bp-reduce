package bpReduce
package transformations
package reduction

import bpReduce.ast.Stmt

/**
 * Reduces a single statement.
 *
 * The possible reduction steps of a statement can be viewed as a `lattice`.
 * The `top` element is `Skip` (no-op) and the bottom is the original statement.
 * This trait is acting like an iterator in the lattice.
 *
 * {{{
 *          Skip
 *        xx    xx
 *       xx      xx
 *      xx        xx
 *  l0 := l1     l1 := l0   /\
 *      xx        xx        ¦
 *       xx      xx         ¦ `reduce` moves up
 *        xx    xx          ¦
 *    l0, l1 := l1, l0
 *
 *    `advance`  --->
 *    moves right
 * }}}
 *
 * Putting `Skip` at the top (and not {}) has practical advantages:
 * if a statement is eliminated, all jump targets must be rewired.
 * If this is done later, it can be separated completely from the reduction process.
 *
 * The reduction algorithm stops if we arrive at the rightmost element or at the top or
 * if no valid program is found during along this path.
 */
trait StmtReducer {

  /**
   * Design choice: we could just say that a reducer that can not reduce a statement
   * just once should not be generated at all. If we do that, we have a bootstrap problem
   * since a reducer must create the next reducer, so the first reducer must be created
   * outside...
   *
   * @return current reduction. Note that it returns also in derived classes
   *         [[Stmt]] because we explicitly return `Skip` to denote removal.
   *         If no reduction is possible, `None` is returned.
   */
  def current: Option[Stmt]

  /**
   * Goes one step up towards `Skip`. The next statement will be simpler than
   * the current one.
   *
   * E.g., starting from `l0, l1 := l1, l0`:
   * if `l0 := l1` was proposed last, it will propose now `Skip`.
   *
   * @return A [[StmtReducer]] that can produce the reduced statement.
   */
  def reduce: Option[StmtReducer]

  /**
   * Goes one step towards the right in the lattice. Thus the next
   * statement will be an alternative reduction to the one that is
   * now proposed. It will not be simpler though.
   *
   * E.g., starting from `l0, l1 := l1, l0`:
   * if `l0 := l1` was proposed last, it will propose now `l1 := l0`.
   *
   * @return A [[StmtReducer]] that can produce the reduced statement.
   */
  def advance: Option[StmtReducer]
}

object StmtReducer {
  val Empty = new StmtReducer {

    def current = None

    def reduce = None

    def advance = None
  }

}