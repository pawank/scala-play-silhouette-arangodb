package utils.extraction

import java.io.{ File, FileInputStream, InputStream }

import org.apache.tika.metadata.Metadata
import org.apache.tika.parser.AutoDetectParser
import org.apache.tika.sax.ToXMLContentHandler
import org.xml.sax.ContentHandler

object TikaExtraction {
  def parseAsHtml(filename: String): Either[String, String] = {
    try {
      val handler: ContentHandler = new ToXMLContentHandler()
      val parser: AutoDetectParser = new AutoDetectParser()
      val metadata: Metadata = new Metadata()
      val initialFile: File = new File(filename)
      val stream: InputStream = new FileInputStream(initialFile)
      parser.parse(stream, handler, metadata)
      Right(handler.toString())
    } catch {
      case e: Exception =>
        val error = utils.Utils.stackTrace(e)
        Left(error)
    }
  }
}
