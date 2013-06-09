import java.io.{File, FileInputStream, FileOutputStream}
import scala.collection.mutable.HashMap
import scala.util.Random

object Letterpress {
  // games are stored by game id
  val games = new HashMap[String, Letterpress]

  // partially constructed game that needs another player
  var partial: Option[(String, Int)] = None

  // Store trie here so we do not reallocate and parse the general trie at every new game
  private val file = new File("trie.txt")
  private val in = new FileInputStream(file)
  private val bytes = new Array[Byte](file.length.toInt)
  in.read(bytes)
  val trie = Trie.deserialize(new String(bytes))

  def apply (h1: String, p1: Int) (h2: String, p2: Int) : Letterpress =
    new Letterpress(new Player(h1, p1), new Player(h2, p2))

  /* Handles move requests. Returns the body of a response and the second player
   * or None on error. */
  def moveHandler (params: List[String]) : Option[(String, (String, Int))] = {

    /* Parses a move list from a string, or returns None.
     * If successful the result has no duplicates. */
    def parseMove (str: String) : Option[List[(Int, Int)]] = {
      val move = List.tabulate(str.length / 2) (i =>
        (str.charAt(2*i).getNumericValue, str.charAt(2*i + 1).getNumericValue))
      if (move.distinct.length != move.length ||
          move.exists(p => (p._1 < 0 || p._1 > 4) || (p._2 < 0 || p._2 > 4)))
        None
      else
        Some(move)
    }

    params.flatMap(_.split("=")) match {
      case List("game", g, "player", p, "move", m) =>
        games.get(g) match {
          case None => None
          case Some(game) =>
            parseMove(m) match {
              case None => None
              case Some(move) =>
                game.tryMove(p)(move) match {
                  case None => None
                  case Some(msg) => {
                    val p2 = game.getOtherPlayer(p)
                    Some((msg, (p2.host, p2.port)))
                  }
                }
            }
        }
      case _ => None
    }
  }

  /* Handles creation of new games. Returns the body of responses and the address
   * for the other player or None if invalid request or no other player. */
  def newGameHandler (params: List[String]) (h1: String, p1: Int) :
                                Option[(String, Option[((String, Int), String)])] = {
    params.flatMap(_.split("=")) match {
      case Nil =>
        partial match {
          case None => {
            partial = Some((h1, p1))
            Some(("Hold the fuck up. We're looking for a game...", None))
          }
          case Some((h, p)) => {
            partial = None
            val game = Letterpress(h, p)(h1, p1)
            games.update(game.id, game)
            val gString = "game=" + game.id + "&" + game // ha
            Some((gString + "&player=" + game.p2.id + "&p1=false",
                 Some(((h, p), gString + "&player=" + game.p1.id + "&p1=true"))))
          }
        }
      case List("host", h2, "port", p2) =>
        val game = Letterpress(h1, p1)(h2, p2.toInt)
        games.update(game.id, game)
        val gString = "game=" + game.id + "&" + game // ha
        Some((gString + "&player=" + game.p1.id + "&p1=true",
             Some(((h2, p2.toInt), gString + "&player=" + game.p2.id + "&p1=false"))))
      case _ => None
    }
  }
}

// An object that represents a game of Letterpress
class Letterpress (val p1: Player, val p2: Player) {

  // A game id is a psuedo-random string of 16 alphanumeric characters
  val id = new String(Random.alphanumeric.take(16).toArray)
  val board = Board.genBoard // board views blue as p1 and red as p2
  var p1Turn = true
  private val trie = Letterpress.trie.subtrie(board.getChars).get // cannot be None

  def getOtherPlayer (pId: String) : Player = {
    pId match {
      case p1.id => p2
      case p2.id => p1
      case _ => throw new IllegalArgumentException("Invalid player id")
    }
  }

  def tryMove (pId: String) (move: List[(Int, Int)]) : Option[String] = {
    (p1Turn, pId) match {
      case (true, p1.id) => {
        val word = board.getWord(move)
        if (trie.remove(word)) {
          board.makeMove(move)(true)
          board.gameover match {
            case None => {
              p1Turn = false
              Some(toString)
            }
            case Some(p1Win) => {
              Letterpress.games.remove(id)
              Some(overString(p1Win))
            }
          }
        }
        else None
      }
      case (false, p2.id) => {
        val word = board.getWord(move)
        if (trie.remove(word)) {
          board.makeMove(move)(false)
          board.gameover match {
            case None => {
              p1Turn = true
              Some(toString)
            }
            case Some(p1Win) => {
              Letterpress.games.remove(id)
              Some(overString(p1Win))
            }
          }
        }
        else None
      }
      case _ => None
    }
  }

  def overString (p1Win: Boolean) : String =
    "board=" + board + "&winner=" + (if (p1Win) "p1" else "p2")

  override def toString : String =
    "board=" + board + "&turn=" + (if (p1Turn) "p1" else "p2")

}