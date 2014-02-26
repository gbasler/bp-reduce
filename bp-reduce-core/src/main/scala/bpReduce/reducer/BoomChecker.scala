package bpReduce
package reducer

import org.apache.commons.exec.{ExecuteException, DefaultExecuteResultHandler, DefaultExecutor, CommandLine}
import bpReduce.ast.Program
import bpReduce.writer.Formatter
import org.apache.commons.io.FileUtils
import java.io.File
import scala.collection.JavaConverters._

class BoomChecker extends Checker {

  // TODO: uh plain ugly
  var iteration = 0

  def apply(program: Program): CheckerResult = {
    val execName = """D:\code\boom-dropbox-svn\bin\Debug\boom.exe"""
//    val execName = "boom"
    val content = Formatter(program)
    val candidate: File = new File(s"reduced.$iteration.bp")
    FileUtils.writeLines(candidate, content.asJava)

    val cmdLine = new CommandLine(execName)
    cmdLine.addArgument("-t")
    cmdLine.addArgument(candidate.getAbsolutePath)
    val executor = new DefaultExecutor
    //  executor.setExitValue(1)
    val exitValue = executor.execute(cmdLine)
    val resultHandler = new DefaultExecuteResultHandler {
      override def onProcessComplete(exitValue: Int): Unit = {
        super.onProcessComplete(exitValue)
        println("Boom completed successfully.")
      }

      override def onProcessFailed(e: ExecuteException): Unit = {
        super.onProcessFailed(e)
        println(s"Boom completed with error ${e.getMessage}")
      }
    }
    ???
  }
}
