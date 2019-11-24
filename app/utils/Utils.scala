package utils

import java.io.{ File, FileOutputStream, FileWriter, IOException, InputStream }
import java.lang.management.{ ManagementFactory, OperatingSystemMXBean }
import java.net.{ InetSocketAddress, Socket, SocketAddress, SocketTimeoutException }
import java.util.concurrent.TimeUnit
import java.util.{ Date, Scanner, TimeZone, UUID }

import org.joda.time.format.{ DateTimeFormat, DateTimeFormatter }

import scala.language.implicitConversions
import scala.collection.JavaConversions._
import org.joda.time.{ DateTime, DateTimeZone, Duration, LocalDateTime }

import scala.util.Try
import scala.xml.Node

/**
 * Created by pawan on 19/03/16.
 */
object Utils {
  val FILE_SEPARATOR = System.getProperty("file.separator")
  val TMP_DIR = System.getProperty("java.io.tmpdir")
  def makeCredentialIdentifier(company: String, email: String): String = company.replaceAll("""[^a-zA-Z0-9]+""", "") + ":" + email

  def makeCompanyCode(company: String): String = company.replaceAll("""[^a-zA-Z0-9]+""", "")

  def makeClientCode(company: String): String = company.replaceAll("""[^a-zA-Z0-9\_\-]+""", "")

  /**
   * Get timezone tuple with (full formated timezone with GMT, just timezone ID)
   * @param tz Timezone
   * @return tuple of GMT based time zone and just timezone ID
   */
  def displayTimeZone(tz: TimeZone): (String, String) = {
    val hours: Long = TimeUnit.MILLISECONDS.toHours(tz.getRawOffset().toLong)
    val minutesTmp: Long = TimeUnit.MILLISECONDS.toMinutes(tz.getRawOffset().toLong) - TimeUnit.HOURS.toMinutes(hours)
    // avoid -4:-30 issue
    val minutes: Long = scala.math.abs(minutesTmp)
    if (hours > 0) {
      ("(GMT+%d:%02d) %s".format(hours, minutes, tz.getID()), tz.getID)
    } else {
      ("(GMT%d:%02d) %s".format(hours, minutes, tz.getID()), tz.getID)
    }
  }

  /**
   * Get a list of all time zones
   * e.g. (GMT-12:00) Etc/GMT+12, (GMT-11:00) Etc/GMT+11, (GMT-11:00) Pacific/Midway, (GMT-11:00) Pacific/Niue etc
   * @return
   */
  def getAllTimeZonesList: List[(String, String)] = {
    val ids: Array[String] = TimeZone.getAvailableIDs
    ids.toList.map(id => displayTimeZone(TimeZone.getTimeZone(id)))
  }

  /**
   * @return Current datetime in UTC format "yyyy-MM-dd'T'HH:mm:ss.SSS'Z"
   */
  def currentUTCDateTime = DateTime.now(DateTimeZone.UTC)

  /**
   * @return Current datetime in UTC format "yyyy-MM-dd'T'HH:mm:ss.SSS'Z"
   */
  def currentTimeZoneBasedDateTime(zone: DateTimeZone) = {
    DateTime.now(zone)
  }

  def currentDateWrtMidnight = currentUTCDateTime.withTimeAtStartOfDay()

  def secondsSinceMidnight: Int = {
    val now = currentUTCDateTime
    val midnight = now.withTimeAtStartOfDay()
    val duration: Duration = new Duration(midnight, now)
    duration.toStandardSeconds().getSeconds()
  }

  def getSecondsSinceMidnight(datetime: DateTime): Int = {
    val h = datetime.getHourOfDay
    val m = datetime.getMinuteOfHour
    val s = datetime.getSecondOfMinute
    val t = 3600 * h + 60 * m + s
    t
  }

  /**
   * Takes a camel cased identifier name and returns a displayable name
   *
   * Example:
   *     camelToDisplay("startDate") == "start date"
   * @param name Input to be made displayable
   * @param isCapitalise Whether to capitalise word or not
   */
  def camelToDisplay(name: String, isCapitalise: Boolean = true) = "[A-Z\\d]".r.replaceAllIn(name, { m =>
    " " + { if (isCapitalise) m.group(0).capitalize else m.group(0).toLowerCase }
  }).capitalize

  /**
   * Takes a camel cased identifier name and returns a dot version of the same
   *
   * Example:
   *     camelToDot("startDate") == "start.date"
   * @param name Input to be made displayable
   */
  def camelToDot(name: String) = "[A-Z\\d]".r.replaceAllIn(name, { m =>
    "." + { m.group(0).toLowerCase }
  })

  val yyyyMMDDFormat = new java.text.SimpleDateFormat("dd/MM/yyyy")
  def optDate(str: String): Option[Date] = if (str.size > 0) Some(yyyyMMDDFormat.parse(str)) else None
  def optDateTime(str: String, format: DateTimeFormatter = DateTimeFormat.forPattern("dd/MM/yyyy")): Option[DateTime] = if (str.size > 0) Some(format.parseDateTime(str)) else None
  def asDateTime(str: String, format: DateTimeFormatter = DateTimeFormat.forPattern("dd/MM/yyyy")): DateTime = format.parseDateTime(str)
  def asDateTime(str: String, dateFormat: String): DateTime = {
    val format = DateTimeFormat.forPattern(dateFormat)
    format.parseDateTime(str)
  }

  val dateFormatYYYYMMDD = "dd/MM/yyyy"
  val dateFormatUTC = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z"

  def optDateForSalesSummaryDateRange(str: String, format: DateTimeFormatter = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm")): Option[DateTime] = if (str.size > 0) Some(format.parseDateTime((str))) else None

  def dateTimeAsFormat(date: DateTime, format: String = "dd/MM/yyyy"): String = {
    //val dtf: DateTimeFormatter = DateTimeFormat.forPattern(format)
    date.toDateTime(DateTimeZone.forID("Asia/Kolkata")).toString(format)
  }
  def utcDateTimeAsFormat(date: DateTime, format: String = "dd/MM/yyyy", timeZoneID: String = "Asia/Kolkata"): String = {
    //val dtf: DateTimeFormatter = DateTimeFormat.forPattern(format)
    date.toString(format)
  }

  def getDateStringToDateTimeWithTimeZone(input: String, formatter: DateTimeFormatter = DateTimeFormat.forPattern("dd/MM/yyyy")): DateTime = {
    formatter.withZone(DateTimeZone.forID("Asia/Kolkata")).parseDateTime(input)
  }

  def saveToFile(filename: String, input: Array[Byte]): Option[String] = {
    val stream: FileOutputStream = new FileOutputStream(filename)
    try {
      stream.write(input)
    } finally {
      stream.close()
    }
    println(s"Wrote filename - $filename")
    Some(filename)
  }

  /**
   * This method ensures that the output String has only
   * valid XML unicode characters as specified by the
   * XML 1.0 standard. For reference, please see
   * <a href="http://www.w3.org/TR/2000/REC-xml-20001006#NT-Char">the
   * standard</a>. This method will return an empty
   * String if the input is null or empty.
   *
   * @param in The String whose non-valid characters we want to remove.
   * @return The in String, stripped of non-valid characters.
   */
  def stripNonValidXMLCharacters(in: String): String = {
    val out: StringBuffer = new StringBuffer
    var current: Char = 0
    if (in == null || (("" == in))) {
      ""
    } else {
      var i: Int = 0
      while (i < in.length) {
        {
          current = in.charAt(i)
          if ((current == 0x9) || (current == 0xA) || (current == 0xD) || ((current >= 0x20) && (current <= 0xD7FF)) || ((current >= 0xE000) && (current <= 0xFFFD)) || ((current >= 0x10000) && (current <= 0x10FFFF))) out.append(current)
        }
        ({
          i += 1; i - 1
        })
      }
    }
    out.toString
  }

  def saveToFile(filename: String, input: String, prefix: String = s"${TMP_DIR}${FILE_SEPARATOR}tally_api_service_", appendNewline: Boolean = false): Option[String] = {
    val finalFilename = if (filename.contains(s"${TMP_DIR}${FILE_SEPARATOR}")) filename else { prefix + filename }
    val p = new java.io.PrintWriter(finalFilename)
    try {
      val inputdata = if (appendNewline) { input + "\n" } else input
      p.write(inputdata)
    } catch {
      case e: Exception =>
        e.printStackTrace()
    } finally {
      p.close()
    }
    println(s"Saved file at $finalFilename")
    Some(finalFilename)
  }

  def writeLog(filename: String, input: String, appendNewline: Boolean = false): Option[String] = {
    val fw = new FileWriter(filename, true)
    try {
      fw.write(if (appendNewline) { input + "\n" } else input)
    } finally fw.close()
    Some(filename)
  }

  def getFileContent(filename: String): Option[String] = {
    try {
      if (new java.io.File(filename).exists()) {
        Some(scala.io.Source.fromFile(filename).mkString)
      } else None
    } catch {
      case e: Exception =>
        e.printStackTrace()
        None
    }
  }
  def deleteFile(filename: String): Boolean = {
    try {
      new File(filename).delete()
      true
    } catch {
      case e: Exception =>
        e.printStackTrace()
        false
    }
  }

  def toDoubleUI(d: Double, precision: Int = 2): String = {
    val head :: tail = "%.2f".format(d).toDouble.toString.split("""\.""").toList
    head + "." + (tail.mkString + "00000000").take(precision)
  }
  def `a.b`(a: String, b: String): String = a + "." + b

  def equalsOptions[T](opt1: Option[T], opt2: Option[T]): Boolean = PartialFunction.cond((opt1, opt2)) { case (Some(x), Some(y)) => x == y }

  def equalsStringOptions(opt1: Option[String], opt2: Option[String]): Boolean = PartialFunction.cond((opt1, opt2)) { case (Some(x), Some(y)) => x.equals(y) }

  def optionValueOrEmpty(opt: Option[String]): String = opt.getOrElse("")

  def md5(text: String): String = {
    import java.security.MessageDigest
    val digest = MessageDigest.getInstance("MD5")
    digest.digest(text.getBytes).map("%02x".format(_)).mkString
  }

  implicit def stackTrace(exp: Throwable): String = {
    import java.io.PrintWriter
    import java.io.StringWriter

    val sw: StringWriter = new StringWriter();
    val pw: PrintWriter = new PrintWriter(sw)
    exp.printStackTrace(pw)
    sw.toString()
  }

  def datetimeToUIDisplay(datetime: DateTime): String = {
    val formatS = new java.text.SimpleDateFormat("dd MMM yyyy hh:mm:ss a")
    formatS.format(datetime.toDate)
  }

  def encodeUrl(data: String): String = {
    import java.nio.charset.{ StandardCharsets => SC }
    import play.utils.UriEncoding
    UriEncoding.encodePathSegment(data, SC.US_ASCII.name)
  }

  def decodeUrl(data: String): String = {
    import java.nio.charset.{ StandardCharsets => SC }
    import play.utils.UriEncoding
    UriEncoding.decodePath(data, SC.US_ASCII.name)
  }

  def getSimpleUID = UUID.randomUUID().toString.take(8)
  def encodeQueryString(input: String): String = {
    import java.net.{ URLDecoder, URLEncoder }
    URLEncoder.encode(input, "UTF-8")
  }

  def decodeQueryString(input: String): String = {
    import java.net.{ URLDecoder, URLEncoder }
    URLDecoder.decode(input, "UTF-8")
  }

  /**
   * isAlive Utility
   *
   * @param hostName
   * @param port
   * @return boolean - true/false
   */
  def isSocketAlive(hostName: String, port: Int): Boolean = {
    //timeout in ms
    val timeout: Int = 2000
    // Creates a socket address from a hostname and a port number
    val socketAddress: SocketAddress = new InetSocketAddress(hostName, port)
    val socket: Socket = new Socket()
    try {
      socket.connect(socketAddress, timeout)
      socket.close()
      true;
    } catch {
      case e: SocketTimeoutException =>
        System.out.println("SocketTimeoutException " + hostName + ":" + port + ". " + e.getMessage())
        false
      case e: IOException =>
        System.out.println(
          "IOException - Unable to connect to " + hostName + ":" + port + ". " + e.getMessage())
        false
      case e: Exception =>
        false
    }
  }

  def getRandomColor(): String = {
    val letters = "0123456789ABCDEF".split("").toList
    var color = "#"
    (0 to 6).map(i => {
      val x = java.lang.Math.floor(java.lang.Math.random() * 16)
      color += letters(x.toInt)
    })
    color
  }

  def generateRandomColors(countOfColors: Int = 1): List[String] = {
    (0 to countOfColors).map(x => getRandomColor()).toList
  }

  def generateRandomRGBColors(countOfColors: Int = 1): List[String] = {
    val op = 0.8
    generateRandomColors(countOfColors).map(x => {
      val r = Integer.valueOf(x.substring(1, 3), 16)
      val g = Integer.valueOf(x.substring(3, 5), 16)
      val b = Integer.valueOf(x.substring(5, 7), 16)
      s"rgba($r,$g,$b,$op)"
    })
  }

  def makeDateTimeFromString(dt: String): Option[DateTime] = {
    val formats = List("dd-MM-yyyy", "dd/MM/yyyy", "dd-MMMM-yyyy", "MM/dd/yyyy", "dd/MMMM/yyyy")
    formats.map(format => {
      Try {
        optDateTime(dt, DateTimeFormat.forPattern(format))
      } match {
        case scala.util.Success(v) =>
          return v
        case scala.util.Failure(e) =>
      }
    })
    None
  }

  def viewFilePath(image: String): String = {
    println(image)
    image.startsWith("/download/cache/") match {
      case true => image
      case false =>
        val v = image.replaceAll("/download//", "/").replaceAll("cache/", "/download/cache/")
        println("image:" + v)
        v
    }
  }

  var simpleCounter: Int = 0
  def getNextSimpleCounter = {
    simpleCounter += 1
    simpleCounter
  }

  def getVersion() = {
    val v = System.getenv("RAPIDOR_PLANX_VERSION")
    val isLive = if (System.getenv("RAPIDOR_DB_MASTER_NAME") != null) System.getenv("RAPIDOR_DB_MASTER_NAME").equalsIgnoreCase("rapidor_master") else false
    val version = if (v != null) v.trim else "v2.0"
    if (isLive) { version + " GM" } else { version + " QA" }
  }

  def isRapidorPlanxIntegrated() = {
    val v = System.getenv("RAPIDOR_PLANX_INTEGRATION")
    //println(s"RAPIDOR_PLANX_INTEGRATION - $v")
    if (v != null) v.trim.equalsIgnoreCase("1") else false
  }

  def isStandardRateOption(condition1: Option[String], condition2: Option[String]): Boolean = {
    val isStandardRatesToBeUsed: Boolean = (for {
      pl1 <- condition1
      pl2 <- condition2
      pl <- {
        Some(pl1.equalsIgnoreCase("standard rates") || pl2.equalsIgnoreCase("standard rates") || pl1.equalsIgnoreCase("standard_rates") || pl2.equalsIgnoreCase("standard_rates"))
      }
    } yield pl).getOrElse(false)
    isStandardRatesToBeUsed
  }

  def getDoubleValue(data: String, name: String = ""): Either[String, Tuple2[Double, String]] = {
    Try {
      //println("trying to get double value for input = " + data)
      data.replaceAll(""",""", "").toDouble
    } match {
      case scala.util.Success(v) => Right((v, ""))
      case scala.util.Failure(e) =>
        //e.printStackTrace()
        //println(s"Exception: ${e.getMessage} for input data - $data")
        val tokens = data.trim.replaceAll(""",""", "").split("""\s+|\/""").toList
        //val tokens = data.trim.split("""\s+|\/|\-""").toList
        //println(s"Tokens for data - $data == $tokens")
        tokens match {
          case qty :: tail =>
            //println(qty, tail)
            if (qty == "") Left("0") else {
              try {
                Right((qty.trim.toDouble, tail.mkString("").trim))
              } catch {
                case e: Exception =>
                  Left("0")
              }
            }
          case _ =>
            Left(e.getMessage)
        }
    }
  }

  def asPrice(data: String): Double = getDoubleValue(data) match {
    case Right(v) => v._1
    case Left(e) => 0.0
  }

  def asPriceUnit(data: String): (Double, String) = getDoubleValue(data) match {
    case Right(v) => (v._1, v._2)
    case Left(e) => (0.0, "")
  }

  def asOptValue(data: String): Option[String] = if (data.isEmpty) None else Some(data)

  def escapeProductName(name: String): String = {
    name.replaceAll("""\"""", """&quot;""")
  }
  def unescapeProductName(name: String): String = {
    name.replaceAll("""&quot;""", """\"""")
  }

  def inputStreamToString(inputStream: InputStream): String =
    {
      val scanner: Scanner = new Scanner(inputStream, "UTF-8").useDelimiter("\\A")
      val string = if (scanner.hasNext()) scanner.next() else ""
      scanner.close()
      string
    }

  def checkProgramStatusInWindows(programName: String) = {
    try {
      val processBuilder = new ProcessBuilder("tasklist.exe")
      val process = processBuilder.start
      val tasksList = inputStreamToString(process.getInputStream)
      tasksList.contains(programName)
    } catch {
      case e: Exception =>
        e.printStackTrace()
        val msg = stackTrace(e)
        if (msg.contains("java.io.IOException: Cannot run program") && msg.contains("No such file or directory")) {
          false
        } else throw new Exception(s"Windows task manager not found or running.")
    }
  }

  def isPortOpen(port: Int): Boolean = {
    import java.io.IOException
    import java.net.DatagramSocket
    import java.net.ServerSocket
    var ss: ServerSocket = null
    var ds: DatagramSocket = null
    var status = false
    try {
      ss = new ServerSocket(port)
      ss.setReuseAddress(true)
      ds = new DatagramSocket(port)
      ds.setReuseAddress(true)
      status = true
    } catch {
      case e: IOException =>
        e.printStackTrace()
        val err = stackTrace(e)
        if (err.contains("java.net.BindException: Address already in use")) {
          status = true
        }
    } finally {
      if (ds != null) ds.close()
      if (ss != null) try
        ss.close()
      catch {
        case e: IOException =>
        /* should not be thrown */
      }
    }
    status
  }
  def getRapidorHostname(domain: String, appendHostname: Boolean = false): String = {
    val hostname = s"https://${domain}.rapidor.co"
    if (appendHostname) { hostname + "?hostname=" + domain + ".rapidor.co" } else hostname
  }

  def getMonthStartEndDates(date: DateTime, isDayStartEndNeeded: Boolean): (DateTime, DateTime) = {
    isDayStartEndNeeded match {
      case false =>
        val dt = new DateTime().withYear(date.getYear).withMonthOfYear(date.getMonthOfYear)
        val start = dt.withDayOfMonth(1).withTimeAtStartOfDay()
        val end = start.plusMonths(1).minusMillis(1)
        (start, end)
      case true =>
        val dt = new DateTime().withYear(date.getYear).withMonthOfYear(date.getMonthOfYear).withDayOfMonth(date.getDayOfMonth)
        val start = dt.withHourOfDay(0).withMinuteOfHour(0).withSecondOfMinute(0).withMillisOfSecond(1)
        val end = start.plusDays(1).minusSeconds(1)
        (start, end)
    }
  }

  def getMonthStartEndDatesAsString(date: DateTime, isDayStartEndNeeded: Boolean, format: String = "yyyyMMdd"): (String, String) = {
    isDayStartEndNeeded match {
      case false =>
        val dt = new DateTime().withYear(date.getYear).withMonthOfYear(date.getMonthOfYear)
        val start = dt.withDayOfMonth(1).withTimeAtStartOfDay()
        val end = start.plusMonths(1).minusMillis(1)
        (dateTimeAsFormat(start, format = format), dateTimeAsFormat(end, format))
      case true =>
        val dt = new DateTime().withYear(date.getYear).withMonthOfYear(date.getMonthOfYear).withDayOfMonth(date.getDayOfMonth)
        val start = dt.withHourOfDay(0).withMinuteOfHour(0).withSecondOfMinute(0).withMillisOfSecond(1)
        val end = start.plusDays(1).minusSeconds(1)
        (dateTimeAsFormat(start, format = format), dateTimeAsFormat(end, format))
    }
  }

  def getMonthStartEndDatesInLocationTZ(date: DateTime, isDayStartEndNeeded: Boolean): (DateTime, DateTime) = {
    isDayStartEndNeeded match {
      case false =>
        val dt = new DateTime().withYear(date.getYear).withMonthOfYear(date.getMonthOfYear)
        val start = dt.withDayOfMonth(1).withTimeAtStartOfDay()
        val end = start.plusMonths(1).minusDays(1)
        (start, end)
      case true =>
        val dt = new DateTime().withYear(date.getYear).withMonthOfYear(date.getMonthOfYear).withDayOfMonth(date.getDayOfMonth)
        val start = dt.withHourOfDay(0).withMinuteOfHour(0).withSecondOfMinute(0).withMillisOfSecond(1)
        val end = start.plusDays(1).minusSeconds(1)
        (start, end)
    }
  }

  def getFinancialYearMonthDates(): List[(DateTime, DateTime)] = {
    val APRIL_NO = 4
    val current = new DateTime()
    val year = current.year().get()
    val yearDate = if (current.monthOfYear().get() > APRIL_NO) current.withMonthOfYear(APRIL_NO) else current.withYear(year - 1).withMonthOfYear(APRIL_NO)
    (0 until 12).map(no => {
      val dt = yearDate.plusMonths(no)
      getMonthStartEndDatesInLocationTZ(dt, isDayStartEndNeeded = false)
    }).toList
  }

  def getSystemInfo() = {
    val operatingSystemMXBean: OperatingSystemMXBean = ManagementFactory.getOperatingSystemMXBean()
    val cpu = operatingSystemMXBean.getSystemLoadAverage() / operatingSystemMXBean.getAvailableProcessors()
    scala.math.round(cpu * 100)
  }
}
