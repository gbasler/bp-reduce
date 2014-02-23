package bpReduce.reducer

import org.apache.commons.exec.{ExecuteException, DefaultExecuteResultHandler, DefaultExecutor, CommandLine}
import bpReduce.Resources
import bpReduce.ast.Program

class BoomChecker extends Checker {

  def apply(program: Program): CheckerResult = {
    val execName = "boom"
    val file = Resources.getFileForUrlOrFile("trace_WP_bug2/main.bp")

    //  Map map = new HashMap();
    //  map.put("file", new File("invoice.pdf"));
    val cmdLine = new CommandLine(execName)
    cmdLine.addArgument("-t")
    cmdLine.addArgument(file.getAbsolutePath)
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
