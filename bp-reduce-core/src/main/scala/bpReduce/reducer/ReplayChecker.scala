package bpReduce
package reducer

import org.apache.commons.io.FileUtils
import java.io.File
import scala.collection.JavaConverters._
import bpReduce.ast.Program
import org.apache.commons.lang.StringUtils
import bpReduce.writer.Formatter
import bpReduce.reducer.CheckerResult.{Reject, Accept}
import bpReduce.util.BooleanPrograms

final class ReplayChecker(outputChecker: OutputChecker,
                          replays: ReplayChecker.Replay,
                          verbose: Boolean) extends Checker {

  def apply(program: Program,
            iteration: Int): CheckerResult = {

    def error: Nothing = {
      Formatter.writeToFile(program, new File("missing-in-action.bp"))
      sys.error(s"Could not find program in replays.")
    }

    val content = Formatter(program)
    val (fileName, output) = replays.getOrElse(content, error)

    val result = outputChecker(output)
    if (verbose)
      result match {
        case Accept => println(s"$fileName: accepted")
        case Reject => println(s"$fileName: rejected")
      }
    result
  }
}

object ReplayChecker {

  /**
   * TODO
   * Storing the parsed programs never worked for me.
   * Somehow the AST of the original program is different
   * when the reduced program is created compared to writing it
   * to disk and reading it back. Even running the simplifier over it
   * did not help.
   * Somehow the program is parsed differently...
   * However writing, reading and writing the program resulted in the same progrm on disk...
   */
  type Replay = Map[IndexedSeq[String], (String, String)]

  def apply(outputChecker: OutputChecker,
            path: File = new File("."),
            verbose: Boolean = true) = {
    val files = FileUtils.listFiles(path, Array(BooleanPrograms.Suffix, ProgramCache.LogSuffix), true).asScala.toIndexedSeq
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

    new ReplayChecker(outputChecker, replays, verbose)
  }
}