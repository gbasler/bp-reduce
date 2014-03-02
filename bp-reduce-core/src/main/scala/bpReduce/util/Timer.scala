package bpReduce
package util


object Timer {

  /**
   * Run the action inside a timer, and return the time elapsed together with the result of the action.
   *
   * @return (elapsedMs, a)
   */
  def timed[A](action: => A): (Double, A) = {
    val start = System.nanoTime
    val a = action
    val end = System.nanoTime
    val elapsedNs = end - start
    (elapsedNs / 1e6, a)
  }
}