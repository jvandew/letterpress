import java.io.{FileInputStream, FileOutputStream}

object TrieBuilder {

  /* Builds a trie from a file of words and serializes it to disk.
   * Requires as input the name of a file with newline separated words in it
   * and a name for the new trie file. */
  def main (args: Array[String]) : Unit = {

    // check for file names as input
    if (args.length != 2)
      System.exit(1)

    val in = new FileInputStream(args(0))
    val trie = new Trie(Nil)

    var word = ""
    var byte = in.read

    while (byte != -1) {
      if (byte.toChar == '\n') {
        trie.insert(word)
        word = ""
      } else
        word = word + byte.toChar
      byte = in.read
    }
    if (word != "") trie.insert(word)

    new FileOutputStream(args(1)).write(trie.toString.getBytes)
  }

}