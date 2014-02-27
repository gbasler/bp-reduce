package bpReduce
package reducer

import org.apache.commons.exec._
import bpReduce.writer.Formatter
import org.apache.commons.io.FileUtils
import java.io.{FileOutputStream, File}
import scala.collection.JavaConverters._
import org.apache.commons.io.output.{TeeOutputStream, ByteArrayOutputStream}
import bpReduce.ast.Program

class BoomChecker(errorLine: String) extends Checker {

  // TODO: uh plain ugly
  var iteration = 0

  val args = Seq("-t", "--threadbound", "3")

  def apply(program: Program): CheckerResult = {
    iteration += 1

    //    val execName = """D:\code\boom-dropbox-svn\bin\Debug\boom.exe"""
    //    val execName = """D:\code\boom-build\bin\Debug\boom.exe"""
    val execName = """/Users/geri/Documents/boom-svn-build-debug/bin/boom"""
    val content = Formatter(program)
    val fileName = s"reduced.$iteration.bp"
    val candidate: File = new File(fileName)
    FileUtils.writeLines(candidate, content.asJava)

    val cmdLine = new CommandLine(execName)
    cmdLine.addArguments(args.toArray)
    cmdLine.addArgument(candidate.getAbsolutePath)
    val executor = new DefaultExecutor
    val outputStream = new ByteArrayOutputStream
    val log = new FileOutputStream(ProgramCache.logFileName(iteration))
    val tee = new TeeOutputStream(outputStream, log)
    val streamHandler = new PumpStreamHandler(tee)
    executor.setStreamHandler(streamHandler)
    executor.setExitValues(Array(0, 1, -2, 134))
    val exitValue = executor.execute(cmdLine)
    val output = outputStream.toString
    if (output.contains(errorLine)) {
      println(s"$fileName: accepted")
      CheckerResult.Accept
    } else {
      println(s"$fileName: rejected")
      CheckerResult.Reject
    }
  }
}
