package bpReduce
package reducer

import org.specs2.mutable.Specification
import java.nio.file.Paths
import bpReduce.reader.BooleanProgramParser
import org.apache.commons.io.{FilenameUtils, FileUtils}
import bpReduce.util.BooleanPrograms
import java.io.File
import scala.collection.JavaConverters._
import org.apache.commons.io.filefilter.{AbstractFileFilter, WildcardFileFilter, SuffixFileFilter, FileFilterUtils}

class ProgramCacheTest extends Specification {
  "failing" in {
    val outputChecker = ErrorOutputChecker.Default
    val dir = Paths.get("/Users/geri/Documents/bp-reduce/bp-reduce-core/src/test/resources/cache")
    val cache = ProgramCache.fromCurrentDir(outputChecker, dir)
    val content = cache.cache.head._1

    val bigCache = ProgramCache.fromCurrentDir(outputChecker)
    val program = new BooleanProgramParser().parse(content.mkString("\n"))
    val result = bigCache.check(program)

    val files = FileUtils.listFiles(new File("."), Array(BooleanPrograms.Suffix), true).asScala.toIndexedSeq
    for {
      file <- files
    } {
      val content0 = FileUtils.readFileToString(file).lines.toIndexedSeq
      if (content0 == content) {
        println(s"Found in ${file.getPath}.")
      }

    }

    ok
  }

  "list dirs" in {
    //    val fileFilter = new SuffixFileFilter(Array(BooleanPrograms.Suffix).map("." + _))

    val wildcard = "????-??-??-??-??-??"
    val fileFilter = new AbstractFileFilter {
      override def accept(dir: File, name: String): Boolean = {
        FilenameUtils.wildcardMatch(dir.getName, wildcard)
      }
    }

    val dirFilter = new WildcardFileFilter(wildcard)
    val candidates = FileUtils.listFiles(new File("."), fileFilter, dirFilter).asScala.toIndexedSeq
    println(candidates.mkString("\n"))
    ok
  }

  "wildcards" in {
    val wildCard = "????-??-??-??-??-??"
    FilenameUtils.wildcardMatchOnSystem("2014-04-03-22-55-31", wildCard) must beTrue
    FilenameUtils.wildcardMatchOnSystem("2014-04-03-22-55", wildCard) must beFalse
    FilenameUtils.wildcardMatchOnSystem("reduced.243.bp", wildCard) must beFalse
  }
}
