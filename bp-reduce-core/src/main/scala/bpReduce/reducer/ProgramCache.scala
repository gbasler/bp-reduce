package bpReduce
package reducer

import bpReduce.ast.Program
import scala.collection.mutable
import bpReduce.writer.Formatter
import org.apache.commons.io.FileUtils
import bpReduce.util.BooleanPrograms
import org.apache.commons.lang.StringUtils
import java.nio.file.{Paths, Path}
import scala.collection.JavaConverters._
import bpReduce.reducer.CheckerResult.{Reject, Accept}

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
final class ProgramCache(cache: mutable.Map[IndexedSeq[String], CacheState]) {

  def this() = {
    this(mutable.Map.empty[IndexedSeq[String], CacheState])
  }

  def add(program: Program, state: CacheState) = {
    val content = Formatter(program)
    require(!cache.contains(content))
    cache += content -> state
  }

  def check(program: Program): CacheState = {
    import CacheState._
    val content = Formatter(program)
    cache.get(content) match {
      case Some(state) => state
      case None        => Unknown
    }
  }
}

object ProgramCache {
  val LogSuffix = "log"

  def logFileName(iteration: Int): String = s"reduced.$iteration.$LogSuffix"

  def loadCandidatesAndLogs(dir: Path): Map[IndexedSeq[String], (String, String)] = {
    val files = FileUtils.listFiles(dir.toFile, Array(BooleanPrograms.Suffix, ProgramCache.LogSuffix), true).asScala.toIndexedSeq
    val (candidates, logs) = files.partition(_.getName.endsWith(s".${BooleanPrograms.Suffix}"))
    val candidatesAndLogs = candidates.flatMap {
      c =>
        val log = s"${StringUtils.removeEnd(c.getName, BooleanPrograms.Suffix)}${ProgramCache.LogSuffix}"
        logs.find(_.getName == log).map {
          log => c -> log
        }
    }

    val replays: Map[IndexedSeq[String], (String, String)] = {
      for {
        (candidateFile, logFile) <- candidatesAndLogs
      } yield {
        val content = FileUtils.readFileToString(candidateFile)
        val log = FileUtils.readFileToString(logFile)
        content.lines.toIndexedSeq ->(candidateFile.getName, log)
      }
    }.toMap

    replays
  }

  /**
   * Populate cache from all bp files and log files that can be found.
   * Useful in order to debug the program reducer itself.
   */
  def fromCurrentDir(outputChecker: OutputChecker): ProgramCache = {
    val cache = mutable.Map.empty[IndexedSeq[String], CacheState]

    val replays: Map[IndexedSeq[String], (String, String)] = loadCandidatesAndLogs(Paths.get("."))
    for {
      (content, (fileName, output)) <- replays
    } {
      cache += (content -> (outputChecker(output) match {
        case Accept => CacheState.Accepted
        case Reject => CacheState.Rejected
      }))

    }
    new ProgramCache(cache)
  }

}