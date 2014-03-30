package bpReduce
package reducer

import java.io.File
import bpReduce.ast.Program
import bpReduce.writer.Formatter
import bpReduce.reducer.CheckerResult.{Reject, Accept}
import java.nio.file.{Paths, Path}

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
        case Accept => println(s"[$iteration] $fileName: √")
        case Reject => println(s"[$iteration] $fileName: -")
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
            path: Path = Paths.get("."),
            verbose: Boolean = true) = {

    val replays = ProgramCache.loadCandidatesAndLogs(path)
    new ReplayChecker(outputChecker, replays, verbose)
  }
}