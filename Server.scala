import java.io.{File, FileInputStream, FileOutputStream, InputStream, OutputStream}
import java.net.{ServerSocket, Socket}
import scala.collection.immutable.Seq

object Server {
  val port = 9001

  val bad_response = ("HTTP/1.1 400 BAD REQUEST\r\n" +
                      "Content-Type: text/plain\r\n" +
                      "Content-Length: 18\r\n" +
                      "\r\n" +
                      "You had ONE job...\r\n")
                     .getBytes("UTF-8")

  val empty_response = ("HTTP/1.1 200 OK\r\n" +
                        "Content-Type: text/plain\r\n" +
                        "Content-Length: 54\r\n" +
                        "\r\n" +
                        "Thanks for wasting our time with your useless request.\r\n")
                       .getBytes("UTF-8")

  def ok_response (body: String) : Array[Byte] =
    ("HTTP/1.1 200 OK\r\n" +
     "Content-Type: text/plain\r\n" +
     "Content-Length: " + body.length + "\r\n" +
     "\r\n" +
     body + "\r\n")
    .getBytes("UTF-8")

  // A GET request to / should return a how-to page detailing how the server works.
  def howto_response (body: String) : Array[Byte] =
    ("HTTP/1.1 200 OK\r\n" +
     "Content-Type: text/html\r\n" +
     "Content-Length: " + body.length + "\r\n" +
     "\r\n" +
     body + "\r\n")
    .getBytes("UTF-8")

  // We send POST requests containing move information to players when unsolicited
  def post_request (body: String) : Array[Byte] =
    ("POST / HTTP/1.1\r\n" +
     "User-Agent: letterpress/fake\r\n" +
     "Content-Length: " + body.length + "\r\n" +
     "Content-Type: application/x-www-form-urlencoded\r\n" +
     "\r\n" +
     body + "\r\n")
    .getBytes("UTF-8")

  def handleRequest (in: InputStream) (out: OutputStream) (cHost: String, cPort: Int) : Unit = {

    // parse content length and return it or None if it doesn't exist
    def parseContentLength (headers: String) : Option[Int] = {
      headers.indexOf("Content-Length: ") match {
        case -1 => None
        case ind => {
          val start = ind + "Content-Length: ".length
          val end = headers.indexOf("\r\n", start)
          val num_string = headers.substring(start, end)
          try {
            Some(num_string.toInt)
          } catch {
            case nfe: NumberFormatException => None
          }
        }
      }
    }

    /* Parses the request method and returns it or None if invalid. Note we enforce having at
     * least "[GET/POST] / HTTP/1.1\r\n". Note the length requirement is a bit different for
     * GET vs POST, but the request would have to meet the longer length anyway to be valid. */
    def parseMethod (headers: String) : Option[String] = {
      if (headers.length < 18)
        None
      else {
        // TODO(jacob) find a pretty pattern matching way of doing this
        val reqType = headers.substring(0, 18)
        if (reqType.contains("GET / HTTP/1.1\r\n")) Some("GET")
        else if (reqType.contains("POST / HTTP/1.1\r\n")) Some("POST")
        else None
      }
    }

    /* Parses the body of the request and takes the appropriate action */
    def processData (data: String): Array[Byte] = {
      val body = data.split("&").toList match {
        case "action=new"::kvs =>
          Letterpress.newGameHandler(kvs)(cHost, cPort) match {
            case None => None
            case Some((msg, None)) => Some(msg)
            case Some((msg, Some(((h2, p2), msg2)))) => {
              val player2out = new Socket(h2, p2).getOutputStream
              player2out.write(post_request(msg2))
              Some(msg)
            }
          }
        case "action=move"::kvs =>
          Letterpress.moveHandler(kvs) match {
            case None => None
            case Some((msg, (h2, p2))) => {
              val player2out = new Socket(h2, p2).getOutputStream
              player2out.write(post_request(msg))
              Some(msg)
            }
          }
        case _ => None
      }
      body match {
        case None => bad_response
        case Some(b) => ok_response(b)
      }
    }

    var data = ""
    var byte = in.read
    // read headers
    // TODO(jacob): it may be better to read into a byte array first then convert to string
    while (byte != -1) {
      data = data + byte.toChar
      byte = in.read
      if (data.length >= 4 && byte.toChar == '\n' &&
          data.substring(data.length - 3, data.length) == "\r\n\r") {
        data = data + '\n'
        byte = -1
      }
    }

    parseMethod(data) match {
      case None => out.write(bad_response)
      case Some("GET") => {
        val file = new File("index.html")
        val fIn = new FileInputStream(file)
        val bytes = new Array[Byte](file.length.toInt)
        fIn.read(bytes)
        out.write(howto_response(new String(bytes)))
      }
      case Some("POST") =>
        parseContentLength(data) match {
          case None => out.write(empty_response)
          case Some(0) => out.write(empty_response)
          case Some(len) => {
            data = ""
            byte = in.read
            var read = 1

            // read data from body
            while (byte != -1 && read < len) {
              data = data + byte.toChar
              byte = in.read
              read = read + 1
            }
            data = data + byte.toChar // append last char to body
            out.write(processData(data))
          }
        }
    }
  }

  // for testing purposes
  def printHTTP (str: String) : Unit =
    println(str.replaceAll("\n", "\\n").replaceAll("\r", "\\r"))

  def printRequest (in: InputStream) : Unit = {
    var byte = in.read
    while (byte != -1) {
      byte.toChar match {
        case '\r' => print("\\r")
        case '\n' => print("\\n")
        case c => print(c)
      }
      byte = in.read
    }
  }

  def main (args: Array[String]): Unit = {

    val socket = new ServerSocket(port)

    while (true) {
      val client = socket.accept
      val in = client.getInputStream
      val out = client.getOutputStream

      val debug = if (args.length == 1 && args(0) == "-d") true else false
      val verbose = if (args.length == 1 && args(0) == "-v") true else false

      // If running in request debug mode this call will never finish
      if (debug) printRequest(in)

      handleRequest(in)(out)(client.getInetAddress.getHostName, client.getPort)
      if (verbose) println("\nVERBOSE: read\n")
    }
  }
}