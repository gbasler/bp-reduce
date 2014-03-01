package bpReduce
package reducer

import org.apache.commons.io.FileUtils
import java.io.File
import scala.collection.JavaConverters._
import bpReduce.ast.Program
import scala.collection.immutable.IndexedSeq
import bpReduce.reader.BooleanProgramParser
import org.apache.commons.lang.StringUtils

class ReplayChecker(errorLine: String,
                    replays: Map[Program, String]) extends Checker {

  def apply(program: Program): CheckerResult = {

    val output = replays.getOrElse(program, sys.error(s"Could not find program in replays."))
    if (output.contains(errorLine)) {
      println(s"accepted")
      CheckerResult.Accept
    } else {
      println(s"rejected")
      CheckerResult.Reject
    }
  }
}

object ReplayChecker {

  def apply(errorLine: String, path: File = new File(".")) = {
    val files = FileUtils.listFiles(path, Array("bp", ProgramCache.LogSuffix), true).asScala.toIndexedSeq
    val (candidates, logs) = files.partition(_.getName.endsWith(".bp"))
    val candidatesAndLogs: IndexedSeq[(File, File)] = candidates.flatMap {
      c =>
        val log = s"${StringUtils.removeEnd(c.getName, "bp")}${ProgramCache.LogSuffix}"
        logs.find(_.getName == log).map {
          log => c -> log
        }
    }

    val parser = new BooleanProgramParser()

    val replays: Map[Program, String] = {
      for {
        (candidateFile, logFile) <- candidatesAndLogs
      } yield {
        val content = FileUtils.readFileToString(candidateFile)
        val log = FileUtils.readFileToString(logFile)
        val program = parser.parse(content)
        program -> log
      }
    }.toMap

    new ReplayChecker(errorLine, replays)
  }
}