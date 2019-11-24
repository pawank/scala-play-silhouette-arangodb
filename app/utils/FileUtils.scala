package utils

import ammonite.ops._

object FileUtils {
  def getFiles(folder: String): Seq[Path] = {
    ls ! Path(folder)
  }

  def getFilenames(folder: String): Seq[String] = getFiles(folder).map(_.toString())
}
