import java.net.{BindException, InetAddress, InetSocketAddress, ServerSocket, Socket, SocketException}
import java.util.Scanner

object TestClient {

  var port = 8001
  var servSock = new ServerSocket
  var board = ""
  var game = ""
  var player = ""

  def listen (port: Int) = {
    val listener = new Thread (new Runnable {
      def run : Unit = {
        while (true) {
          try {
            val sock = servSock.accept
            val in = sock.getInputStream

            var data = ""
            var byte = in.read
            while (byte != -1) {
              data = data + byte.toChar
              byte = in.read
              if (data.length >= 4 && byte.toChar == '\n' &&
                  data.substring(data.length - 3, data.length) == "\r\n\r") {
                data = data + '\n'
                byte = -1
              }
            }

            parseContentLength(data) match {
              case None => println("Empty request...")
              case Some(0) => println("Empty request...")
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
                println("POST recieved: " + data)
                readBody(data)
                print(">> ")
              }
            }

            sock.close
          }
          catch {
            case se: SocketException => ()
          }
        }
      }
    })
    listener.start
  }

  def request (body: String) : Array[Byte] =
    ("POST / HTTP/1.1\r\n" +
     "User-Agent: letterpress/fake-client\r\n" +
     "Content-Length: " + body.length + "\r\n" +
     "Content-Type: application/x-www-form-urlencoded\r\n" +
     "\r\n" +
     body + "\r\n")
    .getBytes("UTF-8")

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

  // not safe if game, board, or player is the last param of body
  def readBody (body: String) : Unit = {
    var from = 0
    body.indexOf("game=", from) match {
      case -1 => ()
      case ind => {
        val start = ind + "game=".length
        val end = body.indexOf("&", start)
        game = body.substring(start, end)
        from = end
      }
    }
    body.indexOf("board=", from) match {
      case -1 => ()
      case ind => {
        val start = ind + "board=".length
        val end = body.indexOf("&", start)
        board = body.substring(start, end)
        printBoard
        from = end
      }
    }
    body.indexOf("player=", from) match {
      case -1 => ()
      case ind => {
        val start = ind + "player=".length
        val end = body.indexOf("&", start)
        player = body.substring(start, end)
      }
    }
  }

  // Forcibly try to reuse a socket until successful
  def rebindSocket (sock: Socket) : Unit = {
    sock.setReuseAddress(true)
    try {sock.bind(new InetSocketAddress(port))}
    catch {case be: BindException => rebindSocket(sock)}
  }

  // Forcibly try to reuse a server socket until successful
  def rebindServerSocket (sock: ServerSocket) : Unit = {
    sock.setReuseAddress(true)
    try {sock.bind(new InetSocketAddress(port))}
    catch {case be: BindException => rebindServerSocket(sock)}
  }

  def handleRequest (reqBody: String) : Unit = {
    servSock.close
    val sock = new Socket
    rebindSocket(sock)
    sock.connect(new InetSocketAddress("localhost", 9001))
    val out = sock.getOutputStream
    out.write(request(reqBody))

    val in = sock.getInputStream
    var data = ""
    var byte = in.read
    while (byte != -1) {
      data = data + byte.toChar
      byte = in.read
      if (data.length >= 4 && byte.toChar == '\n' &&
          data.substring(data.length - 3, data.length) == "\r\n\r") {
        data = data + '\n'
        byte = -1
      }
    }

    parseContentLength(data) match {
      case None => println("Empty response...")
      case Some(0) => println("Empty response...")
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
        println(data)
        readBody(data)
      }
    }

    sock.close
    servSock = new ServerSocket
    rebindServerSocket(servSock)
  }

  def printBoard () : Unit = {
    val bList = board.grouped(2).grouped(5)
    bList.foreach (row => {
      println
      row.foreach(cs => print(cs.charAt(0) + " " + cs.charAt(1) + "   "))
      println("\n")
    })
  }

  /* Simple test client. Only handles one game at a time.
   * Requires a local port to bind to */
  def main (args: Array[String]) : Unit = {
    port = args(0).toInt
    servSock.bind(new InetSocketAddress(port))
    listen(port)
    val scan = new Scanner(System.in)

    println("Commands:\nquit\nrequest [req]\nprintBoard [board]\nmoveReq [move]\n")
    while(true) {
      print(">> ")
      scan.nextLine.split(" ").toList match {
        case List("quit") => System.exit(0)
        case List("request", reqBody) => handleRequest(reqBody)
        case List("printBoard") => printBoard
        case List("moveReq", move) =>
          handleRequest("action=move&game=" + game + "&player=" + player + "&move=" + move)
        case _ => println("Bad input. Try again")
      }
    }
  }
}