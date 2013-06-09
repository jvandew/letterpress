import scala.util.Random

// Companion object for the Board class
object Board {

  private val trieChars = Letterpress.trie.numChars
  val charFreqs = Letterpress.trie.charCounts.mapValues(_.toDouble / trieChars).toArray

  /* Generates a new Letterpress game board based on the frequencies of characters
   * in the Letterpress dictionary. */
  def genBoard : Board = {
    val rand = new Random
    val bArray = new Array[Array[(Char, Char)]](5)

    for (i <- 0 to 4) {
      bArray(i) = new Array[(Char, Char)](5)
      for (j <- 0 to 4) {
        var acc = charFreqs(0)._2
        val target = rand.nextDouble
        var index = 1
        while (acc < target && index < charFreqs.length) {
          acc = acc + charFreqs(index)._2
          index = index + 1
        }
        bArray(i)(j) = (charFreqs(index - 1)._1, 'w')
      }
    }

    val board = new Board(bArray)
    // If this board is not solvable generate a different board.
    // Note the recursive tail call will be optimized out
    if (isSolvable(board)) board
    else genBoard
  }

  // Checks to see if this board is in fact solvable
  def isSolvable (board: Board) : Boolean = {
    Letterpress.trie.subtrie(board.getChars) match {
      case None => false
      case Some(trie) => {
        // checks to see if all open characters can be used by the given word list
        def check (open: List[Char]) (words: List[String]) : Boolean =
          (open, words) match {
            case (Nil, _) => true
            case (_, Nil) => false
            case (_, w::ws) => check (open.diff(w)) (ws)
          }

        check (board.getChars) (trie.listWords)
      }
    }
  }
}

/* A board is a 2D array of character pairs. The first character represents the
 * character on the game board at that position, while the second represents its
 * state (b = blue, B = blue secured, r = red, R = red secured, w = neutral).
 * Note we view blue as player 1 and red as player 2. */
class Board (val board : Array[Array[(Char, Char)]]) {

  // Returns a list of characters on this game board
  def getChars : List[Char] = board.flatten.map(chars => chars._1).toList

  /* Returns the word associated with a sequence of indices on this board.
   * Assumes valid input. */
  def getWord (move: List[(Int, Int)]) : String = {
    move match {
      case Nil => ""
      case (i1, i2)::is => board(i1)(i2)._1 + getWord(is)
    }
  }

  /* Returns an option specifying if the game is over. None is not over.
   * Some(true) means player 1 has won. Some(false) means player 2 has won. */
  def gameover () : Option[Boolean] = {
    if (board.forall(_.forall(lc => lc._2 != 'w')))
      if (board.map(_.count(lc => lc._2 == 'B' || lc._2 == 'b')).reduce(_ + _) >= 13)
        Some(true)
      else
        Some(false)
    else
      None
  }

  /* Makes the given move on behalf of the specified player */
  def makeMove (move: List[(Int, Int)]) (p1: Boolean) : Unit = {
    // Fixes the coloring of the board
    // TODO(jacob): make this piece of code less shitty looking somehow
    def recolor (own: Char, ownSet: Char, op: Char, opSet: Char) : Unit = {
      for (i <- 0 to 4) {
        for (j <- 0 to 4) {
          val check = List((i, j), (i, j-1), (i, j+1), (i-1, j), (i+1, j))
                      .filter(xy => (0 <= xy._1 && xy._1 < 5) && (0 <= xy._2 && xy._2 < 5))
          val owned = check.map(xy => board(xy._1)(xy._2)._2 == own ||
                                      board(xy._1)(xy._2)._2 == ownSet)
          val opOwned = check.map(xy => board(xy._1)(xy._2)._2 == op ||
                                        board(xy._1)(xy._2)._2 == opSet)
          board(i)(j) = (board(i)(j)._1,
            if (owned.forall(_ == true)) ownSet
            else if (opOwned.forall(_ == true)) opSet
            else if (owned.head) own
            else if (opOwned.head) op
            else 'w'
          )
        }
      }
    }

    val (own, ownSet, op, opSet) = if (p1) ('b', 'B', 'r', 'R') else ('r', 'R', 'b', 'B')
    move.foreach(i => if (board(i._1)(i._2)._2 != opSet) board(i._1)(i._2) = (board(i._1)(i._2)._1, own))
    recolor(own, ownSet, op, opSet)
  }

  override def toString : String =
    board.flatten.map(chars => chars._1.toString + chars._2).reduce(_ + _)
}