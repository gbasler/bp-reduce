package bpReduce
package reducer

import scala.collection.mutable
import bpReduce.writer.Formatter
import org.apache.commons.io.{FilenameUtils, FileUtils}
import bpReduce.util.BooleanPrograms
import org.apache.commons.lang.StringUtils
import java.nio.file.{Paths, Path}
import scala.collection.JavaConverters._
import bpReduce.reducer.CheckerResult.{Reject, Accept}
import org.apache.commons.io.filefilter._
import java.io.File
import bpReduce.ast.Program

sealed abstract class CacheState

object CacheState {

  case object Accepted extends CacheState

  case object Rejected extends CacheState

  case object Unknown extends CacheState

}

/**
 * Simple program cache that contains all variants of
 * a program that have been checked.
 *
 * Note that we store positive and negative results, since
 * it might happen that a reduced variant corresponds to a positive
 * checked other variant after program simplification.
 *
 * We store just the formatted program, see also [[ReplayChecker]].
 */
final class ProgramCache(val cache: mutable.Map[IndexedSeq[String], (CacheState, Option[String])]) {

  def this() = {
    this(mutable.Map.empty[IndexedSeq[String], (CacheState, Option[String])])
  }

  def add(program: Program, state: CacheState) = {
    val content = Formatter(program)
    require(!cache.contains(content))
    cache += content ->(state, None)
  }

  def check(program: Program): (CacheState, Option[String]) = {
    import CacheState._
    val content = Formatter(program)
    cache.get(content) match {
      case Some(state) => state
      case None        => Unknown -> None
    }
  }
}

object ProgramCache {
  val LogSuffix = "log"

  def logFileName(iteration: Int): String = s"reduced.$iteration.$LogSuffix"

  def loadCandidatesAndLogs(dir: Path,
                            directoryFilter: Boolean = true,
                            verbose: Boolean = false): Map[IndexedSeq[String], (String, String)] = {

    val suffixFilter = new SuffixFileFilter(Array(BooleanPrograms.Suffix).map("." + _))

    val (fileFilter, dirFilter) = if (directoryFilter) {
      val wildcard = "????-??-??-??-??-??"

      val pathFilter = new AbstractFileFilter {
        val reported = mutable.Set[String]()

        override def accept(dir: File, name: String): Boolean = {
          val ok = FilenameUtils.wildcardMatch(dir.getName, wildcard)
          val path = dir.getPath
          if (verbose && ok && !reported.contains(path)) {
            reported += path
            println(s"Adding .bp and .log from <$path>...")
          }
          ok
        }
      }

      val fileFilter = new AndFileFilter(suffixFilter, pathFilter)
      val dirFilter = new WildcardFileFilter(wildcard)
      fileFilter -> dirFilter
    } else {
      suffixFilter -> TrueFileFilter.INSTANCE
    }

    val candidates = FileUtils.listFiles(dir.toFile, fileFilter, dirFilter).asScala.toIndexedSeq

    def logFileNameFor(file: File) = {
      FilenameUtils.removeExtension(file.getPath) + "." + ProgramCache.LogSuffix
    }

    val replays: Map[IndexedSeq[String], (String, String)] = {
      for {
        candidateFile <- candidates
        logFileName = logFileNameFor(candidateFile)
        logFile = new File(logFileName)
        if logFile.exists
      } yield {
        val content = FileUtils.readFileToString(candidateFile)
        val log = FileUtils.readFileToString(logFile)
        content.lines.toIndexedSeq ->(candidateFile.getPath, log)
      }
    }.toMap

    replays
  }

  /**
   * Populate cache from all bp files and log files that can be found.
   * Useful in order to debug the program reducer itself.
   */
  def fromDir(outputChecker: OutputChecker,
              path: Path = Paths.get("."),
              cache: ProgramCache = new ProgramCache,
              directoryFilter: Boolean = true,
              verbose: Boolean = false): ProgramCache = {

    val replays = loadCandidatesAndLogs(path, directoryFilter, verbose)

    for {
      (content, (fileName, output)) <- replays
    } {
      val result = outputChecker(output) match {
        case Accept => CacheState.Accepted
        case Reject => CacheState.Rejected
      }

      cache.cache += (content ->(result, Some(fileName)))
    }

    cache
  }

}