package bpReduce
package main

import java.io.File
import bpReduce.reader.BooleanProgramParser
import org.apache.commons.io.FileUtils
import bpReduce.reducer._
import bpReduce.reduction.Reducers
import scopt.OptionParser
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime
import java.nio.file.FileSystems
import bpReduce.writer.Formatter
import scopt.OptionParser
import bpReduce.reducer.Reducer
import bpReduce.reducer.ReducerConfig
import scala.Some

object Main {
  val name = "bp-reduce"

  def main(args: Array[String]) {
    final case class Config(file: File = new File("."),
                            outFile: File = new File("reduced.bp"),
                            replay: Option[File] = None)

    val parser = new OptionParser[Config](name) {
      head(name)
      opt[File]('r', "replay") valueName "<log-dir>" action {
        (x, c) =>
          c.copy(replay = Some(x))
      } text "directory with logs from a previous run"
      opt[File]('o', "output") valueName "<outfile>" action {
        (x, c) =>
          c.copy(outFile = x)
      } text "directory with logs from a previous run"
      arg[File]("<file>...") required() valueName "<file>" action {
        (x, c) =>
          c.copy(file = x)
      } text "need a file to reduce"
    }
    parser.parse(args, Config()) map {
      config =>
        val content = FileUtils.readFileToString(config.file)
        val program = new BooleanProgramParser().parse(content)
        val outputChecker = ErrorOutputChecker("Assertion failed")
        val checker = config.replay match {
          case Some(replayDir) =>
            ReplayChecker(outputChecker, replayDir)
          case None            =>
            val fmt = DateTimeFormat.forPattern("yyyy-MM-dd-HH-mm-ss")
            val logDir = new DateTime().toString(fmt)
            new BoomChecker(outputChecker, FileSystems.getDefault.getPath(logDir))
        }
        val cfg = ReducerConfig(reducers = Reducers.All, checker = checker, simplify = true)
        val reducer = new Reducer(cfg)
        val reduced = reducer(program)
        Formatter.writeToFile(reduced, config.outFile)
    } getOrElse {
      // arguments are bad, error message will have been displayed
    }
  }
}
