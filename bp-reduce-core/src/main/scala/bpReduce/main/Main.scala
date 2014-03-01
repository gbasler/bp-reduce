package bpReduce
package main

import java.io.File
import bpReduce.reader.BooleanProgramParser
import org.apache.commons.io.FileUtils
import bpReduce.reducer.{ReplayChecker, BoomChecker, ReducerConfig, Reducer}
import bpReduce.reduction.Reducers
import scopt.OptionParser

object Main {
  val name = "bp-reduce"

  def main(args: Array[String]) {
    final case class Config(file: File = new File("."),
                            replay: Option[File] = None)

    val parser = new OptionParser[Config](name) {
      head(name)
      opt[File]('r', "replay") valueName "<log-dir>" action {
        (x, c) =>
          c.copy(replay = Some(x))
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
        val property = "Assertion failed"
        val checker = config.replay match {
          case Some(replayDir) =>
            ReplayChecker(property, replayDir)
          case None    =>
            new BoomChecker(property)
        }
        val cfg = ReducerConfig(reducers = Reducers.All, checker = checker, simplify = true)
        val reducer = new Reducer(cfg)
        reducer(program)
    } getOrElse {
      // arguments are bad, error message will have been displayed
    }
  }
}
