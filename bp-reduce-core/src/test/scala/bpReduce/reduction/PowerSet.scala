package bpReduce
package reduction


import scala.annotation.tailrec
import bpReduce.BaseSpecification

/**
 * Proof-of-concept of power set algorithm that instead of adding elements to
 * sets, is based solely on removal of elements from sets.
 */
class PowerSet extends BaseSpecification {

  "powerset" in {

    // known algorithm
    def incrementalPowerSet[A](s: Set[A]): Set[Set[A]] = s.foldLeft(Set(Set.empty[A])) {
      case (ss, e) => ss ++ ss.map(_ + e)
    }


    def decrementalPowerSet[A](s: Set[A]): Set[Set[A]] = {
      @tailrec
      def decrementalPowerSet(wl: Set[Set[A]],
                              acc: Set[Set[A]]): Set[Set[A]] = {
        if (wl.nonEmpty) {
          val next = for {
            elems <- wl
            r <- elems
            n = elems - r
          } yield n
          decrementalPowerSet(next, acc ++ next)
        } else {
          acc
        }
      }
      // note that biggest set (=initial set) is directly added to result
      decrementalPowerSet(Set(s), Set(s))
    }

    val elems = Set("a", "b", "c", "d", "e")

    val solution = decrementalPowerSet(elems)

    val expected = incrementalPowerSet(elems)
    solution.mkString("\n") must be_==(expected.mkString("\n"))
  }

}
