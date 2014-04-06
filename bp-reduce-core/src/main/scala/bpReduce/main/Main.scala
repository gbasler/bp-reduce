package bpReduce
package main

import java.io.File
import bpReduce.reader.BooleanProgramParser
import org.apache.commons.io.FileUtils
import bpReduce.reducer._
import bpReduce.reduction.Reducers
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime
import java.nio.file.{Paths, FileSystems}
import bpReduce.writer.Formatter
import scopt.OptionParser
import bpReduce.reducer.Reducer
import bpReduce.reducer.ReducerConfig
import scala.Some
import bpReduce.transformations.ProgramSimplifier

object Main {
  val name = "bp-reduce"

  def main(args: Array[String]) {
    final case class Config(file: File = new File("."),
                            outFile: File = new File("reduced.bp"),
                            replay: Option[File] = None,
                            diskCache: Boolean = false,
                            verbose: Boolean = false)

    val parser = new OptionParser[Config](name) {
      head(name)
      opt[File]('r', "replay") valueName "<log-dir>" action {
        (x, c) =>
          c.copy(replay = Some(x))
      } text "directory with logs from a previous run"
      opt[Unit]("disk-cache") action {
        (_, c) =>
          c.copy(diskCache = true)
      } text "does a full recursive search for bp and log files"
      opt[Unit]("verbose") action {
        (_, c) =>
          c.copy(diskCache = true)
      } text "Verbose output"
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
        val outputChecker = ErrorOutputChecker.Default
        val checker = config.replay match {
          case Some(replayDir) =>
            ReplayChecker(outputChecker, replayDir.toPath)
          case None            =>
            val fmt = DateTimeFormat.forPattern("yyyy-MM-dd-HH-mm-ss")
            val logDir = new DateTime().toString(fmt)
            val checker = new BoomChecker(outputChecker, FileSystems.getDefault.getPath(logDir))
            if (config.diskCache) {
              println("Using disk cache.")
              new CachingChecker(checker, ProgramCache.fromDir(outputChecker))
            } else {
              new CachingChecker(checker)
            }
        }
        val cfg = ReducerConfig(reducers = Reducers.All, checker = checker, simplify = true)
        val reducer = new Reducer(cfg)
        val content = FileUtils.readFileToString(config.file)
        val program = new BooleanProgramParser().parse(content)
        val simplified = ProgramSimplifier(program)
        val reduced = reducer(simplified)
        Formatter.writeToFile(reduced, config.outFile)
    } getOrElse {
      // arguments are bad, error message will have been displayed
    }
  }
}
