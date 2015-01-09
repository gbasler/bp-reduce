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
                            smartAcceleration: Boolean = false,
                            verbose: Boolean = false)

    val parser = new OptionParser[Config](name) {
      head(name)
      // the following options are extremely useful when debugging bp-reduce:

      // a replay is a directory that contains .bp and .log files from a previous run
      // if a replay is used, then Boom is never called, instead all problems are
      // expected to be present in the replay
      opt[File]('r', "replay") valueName "<log-dir>" action {
        (x, c) =>
          c.copy(replay = Some(x))
      } text "directory with logs from a previous run"

      // similar to using multiple replays and running Boom for missing problems
      // thus the cache is built up gradually
      opt[Unit]("disk-cache") action {
        (_, c) =>
          c.copy(diskCache = true)
      } text "does a full recursive search for bp and log files"

      opt[Unit]("smart") action {
        (_, c) =>
          c.copy(smartAcceleration = true)
      } text "Use smart acceleration algorithm"
      opt[Unit]("verbose") action {
        (_, c) =>
          c.copy(verbose = true)
      } text "Verbose output"
      opt[File]('o', "output") valueName "<outfile>" action {
        (x, c) =>
          c.copy(outFile = x)
      } text "Output file name"
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
          case None =>
            val fmt = DateTimeFormat.forPattern("yyyy-MM-dd-HH-mm-ss")
            val logDir = new DateTime().toString(fmt)
            val checker = new BoomChecker(outputChecker, FileSystems.getDefault.getPath(logDir))
            if (config.diskCache) {
              println("Using disk cache.")
              new CachingChecker(checker, config.verbose, ProgramCache.fromDir(outputChecker, verbose = config.verbose))
            } else {
              new CachingChecker(checker, config.verbose)
            }
        }
        val cfg = ReducerConfig(reducers = Reducers.All, checker = checker, simplify = true, config.smartAcceleration)
        val reducer = new Reducer(cfg, config.verbose)
        val content = FileUtils.readFileToString(config.file)
        val program = new BooleanProgramParser().parse(content)
        val simplified = ProgramSimplifier(program)
        val reduced = reducer(simplified)
        // TODO: somehow removing variables during reduction
        // compromises the algorithm... why?
        val reducedSimplified = ProgramSimplifier.simplifyVariablesAndFunctions(reduced)
        Formatter.writeToFile(reducedSimplified, config.outFile)
    } getOrElse {
      // arguments are bad, error message will have been displayed
    }
  }
}
