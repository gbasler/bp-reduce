package bpReduce
package reducer

import org.apache.commons.io.FileUtils
import java.io.File
import scala.collection.JavaConverters._
import bpReduce.ast.Program
import bpReduce.reader.BooleanProgramParser
import org.apache.commons.lang.StringUtils
import bpReduce.writer.Formatter
import bpReduce.reducer.CheckerResult.{Reject, Accept}

class ReplayChecker(outputChecker: OutputChecker,
                    replays: Map[Program, (String, String)]) extends Checker {

  def apply(program: Program): CheckerResult = {

    def error: Nothing = {
      Formatter.writeToFile(program, new File("missing-in-action.bp"))
      sys.error(s"Could not find program in replays.")
    }

    val (fileName, output) = replays.getOrElse(program, error)

    val result = outputChecker(output)
    result match {
      case Accept => println(s"$fileName: accepted")
      case Reject => println(s"$fileName: rejected")
    }
    result
  }
}

object ReplayChecker {

  def apply(outputChecker: OutputChecker, path: File = new File(".")) = {
    val files = FileUtils.listFiles(path, Array("bp", ProgramCache.LogSuffix), true).asScala.toIndexedSeq
    val (candidates, logs) = files.partition(_.getName.endsWith(".bp"))
    val candidatesAndLogs = candidates.flatMap {
      c =>
        val log = s"${StringUtils.removeEnd(c.getName, "bp")}${ProgramCache.LogSuffix}"
        logs.find(_.getName == log).map {
          log => c -> log
        }
    }

    val parser = new BooleanProgramParser()

    val replays: Map[Program, (String, String)] = {
      for {
        (candidateFile, logFile) <- candidatesAndLogs
      } yield {
        val content = FileUtils.readFileToString(candidateFile)
        val log = FileUtils.readFileToString(logFile)
        val program = parser.parse(content)
        program ->(candidateFile.getName, log)
      }
    }.toMap

    new ReplayChecker(outputChecker, replays)
  }
}