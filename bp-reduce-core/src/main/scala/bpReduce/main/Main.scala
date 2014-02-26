package bpReduce
package main

import java.io.File
import bpReduce.reader.BooleanProgramParser
import org.apache.commons.io.FileUtils
import bpReduce.reducer.{BoomChecker, ReducerConfig, Reducer}
import bpReduce.reduction.Reducers

object Main {
  val name = "bp-reduce"

  def main(args: Array[String]) {
    final case class Config(file: File = new File("."))

    val parser = new scopt.OptionParser[Config](name) {
      head(name)
      opt[File]('f', "file") required() valueName "<file>" action {
        (x, c) =>
          c.copy(file = x)
      } text "need a file to reduce"
    }
    parser.parse(args, Config()) map {
      config =>
        val content = FileUtils.readFileToString(config.file)
        val program = new BooleanProgramParser().parse(content)
        val checker = new BoomChecker
//        checker(program)
        val cfg = ReducerConfig(reducers = Reducers.All, checker = checker, simplify = true)
        val reducer = new Reducer(cfg)
        reducer(program)
    } getOrElse {
      // arguments are bad, error message will have been displayed
    }
  }
}
