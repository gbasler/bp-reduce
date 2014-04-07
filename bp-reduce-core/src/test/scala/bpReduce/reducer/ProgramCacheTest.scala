package bpReduce
package reducer

import org.specs2.mutable.Specification
import org.apache.commons.io.FilenameUtils

class ProgramCacheTest extends Specification {

  "wildcards" in {
    val wildCard = "????-??-??-??-??-??"
    FilenameUtils.wildcardMatchOnSystem("2014-04-03-22-55-31", wildCard) must beTrue
    FilenameUtils.wildcardMatchOnSystem("2014-04-03-22-55", wildCard) must beFalse
    FilenameUtils.wildcardMatchOnSystem("reduced.243.bp", wildCard) must beFalse
  }
}
