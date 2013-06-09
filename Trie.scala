import scala.collection.mutable.HashMap

trait TrieOps {
  def charCounts : HashMap[Char, Long]
  def copy : TrieOps
  def insert (str: String) : Unit
  def insert (strs: List[String]) : Unit = strs.foreach(insert(_))
  def listWords : List[String]
  def lookup (str: String) : Boolean
  def numChars : Long
  def numWords : Long
  def toString : String
  def remove (str: String) : Boolean
  def remove (strs: List[String]) : Int = strs.map(remove(_)).count(r => r)
  def subtrie (letters: List[Char], binding: Boolean) : Option[TrieOps]
}

// Companion object for tries allowing deserialization
object Trie {

  def deserialize (trieString: String) : Trie = {

    // Takes a string to deserialize and returns a node deserialized and the remainder of the string
    def parseChildren (trie: Trie) (tString: String) : (Trie, String) = {
      tString.headOption match {
        case None => throw new IllegalArgumentException("Malformed trie string")
        case Some('\r') => (trie, tString.tail)
        case Some('\n') => {
          trie.children = ('\n', null) :: trie.children
          parseChildren(trie)(tString.tail)
        }
        case Some(hChar) => {
          val (child, newTS) = parseChildren(new Trie(Nil))(tString.tail)
          trie.children = (hChar, child) :: trie.children
          parseChildren(trie)(newTS)
        }
      }
    }

    parseChildren(new Trie(Nil))(trieString) match {
      case (trie, "") => trie
      case (trie, _) => throw new IllegalArgumentException("Malformed trie string")
    }
  }

}

/* A trie node consists of a list of children and their characters.
 * Sequence termination is represented by a node storing '\n' with no children */
class Trie (var children: List[(Char, Trie)]) extends TrieOps {

  // Return a map of the characters in the trie and their frequencies in the words stored
  def charCounts : HashMap[Char, Long] = {
    val counts = new HashMap[Char, Long]
    val childrenCounts = children.map (charTrie => {
      charTrie._1 match {
        case '\n' => new HashMap[Char, Long]
        case c => {
          val childCounts = charTrie._2.charCounts
          childCounts.get(c) match {
            case None => childCounts.update(c, charTrie._2.numWords)
            case Some(freq) => childCounts.update(c, freq + charTrie._2.numWords)
          }
          childCounts
        }
      }
    })
    childrenCounts.foreach(childCounts =>
      childCounts.foreach (cFreq => {
        counts.get(cFreq._1) match {
          case None => counts.update(cFreq._1, cFreq._2)
          case Some(count) => counts.update(cFreq._1, cFreq._2 + count)
        }
      })
    )
    counts
  }

  // Return a distinct copy of this trie
  def copy : Trie = {
    val newChildren = children.map (charTrie =>
      charTrie match {
        case ('\n', null) => ('\n', null)
        case (c, trie) => (c, trie.copy)
      }
    )
    new Trie(newChildren)
  }

  // Match first character to an existing or new node and then recurse on this node with the tail
  def insert (str: String) : Unit = {
    str.headOption match {
      case None =>
        children.find(charTrie => charTrie._1 == '\n') match {
          case None => children = ('\n', null)::children
          case _ => Unit
        }
      case Some(hChar) =>
        children.find(charTrie => charTrie._1 == hChar) match {
          case None => {
            val newNode = new Trie(Nil)
            children = (hChar, newNode)::children
            newNode.insert(str.tail)
          }
          case Some((c, t)) => t.insert(str.tail)
        }
    }
  }

  // Returns a list of words stored in this trie. No guarantees are made about ordering
  def listWords : List[String] = {
    def listWordsRoot (trie: Trie) (root: String) : List[String] =
      trie.children.flatMap (charTrie => {
        charTrie._1 match {
          case '\n' => List(root)
          case c => listWordsRoot(charTrie._2)(root + c)
        }
      })

    listWordsRoot(this)("")
  }

  def lookup (str: String) : Boolean =
    str.headOption match {
      case None =>
        children.find(charTrie => charTrie._1 == '\n') match {
          case None => false
          case Some(_) => true
        }
      case Some(hChar) =>
        children.find(charTrie => charTrie._1 == hChar) match {
          case None => false
          case Some((c, t)) => t.lookup(str.tail)
        }
    }

  // Compute the number of characters stored in this trie.
  def numChars : Long = {
    val childrenChars = children.map (charTrie => {
      charTrie._1 match {
        case '\n' => 0
        case c => charTrie._2.numWords + charTrie._2.numChars
      }
    })
    childrenChars.reduce(_ + _)
  }

  // Compute the number of words stored in this trie.
  // Note this is Synonymous with the number of '\n' characters stored
  def numWords : Long = {
    val childrenWords = children.map (charTrie => {
      charTrie._1 match {
        case '\n' => 1
        case c => charTrie._2.numWords
      }
    })
    childrenWords.reduce(_ + _)
  }

  /* Removes the given word from the trie. Returns true if the word was removed or
   * false if the word was already not present */
  def remove (str: String) : Boolean = {

    /* Returns the removal result plus whether or not the removed word was the only
     * word stored in the trie (allowing the trie to now be deleted). */
    def remover (trie: Trie) (str: String) : (Boolean, Boolean) =
      str.headOption match {
        case None =>
          trie.children.find (charTrie => charTrie._1 == '\n') match {
            case None => (false, false)
            case Some(child) => {
              trie.children = trie.children.diff(List(child))
              (true, trie.children.isEmpty)
            }
          }
        case Some(hChar) =>
          trie.children.find (charTrie => charTrie._1 == hChar) match {
            case None => (false, false)
            case Some(child) =>
              remover(child._2)(str.tail) match {
                case (true, true) => {
                  trie.children = trie.children.diff(List(child))
                  (true, trie.children.isEmpty)
                }
                case res => res
              }
          }
      }

    remover(this)(str)._1
  }

  /* Returns a subtrie of this trie containing words meeting the following conditions:
   * Case binding == true: only words that contain all characters in the given list
   * Case binding == false: only words that can be built from characters in the given list
   * ... or None if no such subtrie exists */
  def subtrie (letters: List[Char], binding: Boolean = false) : Option[Trie] = {
    letters match {
      case Nil =>
        (binding, children.contains(('\n', null))) match {
          case (true, _) => Some(copy)
          case (false, true) => Some(new Trie(List(('\n', null))))
          case (false, false) => None
        }
      case _ => {
        val childrenOpts = children.map (charTrie => {
          charTrie._1 match {
            case '\n' => if (binding) ('\n', None) else ('\n', Some(null))
            case c =>
              (binding, letters.contains(c)) match {
                case (_, true) => (c, charTrie._2.subtrie(letters.diff(List(c)), binding))
                case (true, false) => (c, charTrie._2.subtrie(letters, binding))
                case (false, false) => (c, None)
              }
          }
        })
        val validOpts = childrenOpts.filter (_._2 match {
          case None => false
          case Some(_) => true
        })
        // Option.get guaranteed safe by previous filter
        val valids = validOpts.map (charTrieOpt => (charTrieOpt._1, charTrieOpt._2.get))
        if (valids.isEmpty) None else Some(new Trie(valids))
      }
    }
  }

  /* Serializes this trie
   * TODO(jacob): find some way to simply write out "tails" as simple strings
   * rather than a series of single child nodes */
  override def toString : String =
    (children.flatMap
      (charTrie => if (charTrie._1 == '\n') "\n"
                   else charTrie._1 + charTrie._2.toString))
    .mkString + '\r'

}