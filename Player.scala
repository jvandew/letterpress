import java.net.Socket
import scala.util.Random

class Player (val host: String, val port: Int) {
  // A game id is a psuedo-random string of 16 alphanumeric characters
  val id = new String(Random.alphanumeric.take(16).toArray)
}