package bpReduce

import java.io.File
import org.springframework.core.io.FileSystemResource
import org.springframework.core.io.DefaultResourceLoader
import org.apache.commons.io.FileUtils

object Resources {
  def loadFileOrUrl(fileOrUrl: String): String = {
    val resource = if (new File(fileOrUrl).exists()) {
      new FileSystemResource(fileOrUrl)
    } else {
      new DefaultResourceLoader().getResource(fileOrUrl)
    }
    FileUtils.readFileToString(resource.getFile)
  }
}