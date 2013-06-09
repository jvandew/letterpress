# TODO(jacob): learn how to write Makefiles...

all: Trie.scala TrieBuilder.scala Board.scala Player.scala Letterpress.scala Server.scala TestClient.scala
	scalac Trie.scala TrieBuilder.scala Board.scala Player.scala Letterpress.scala Server.scala TestClient.scala

client: TestClient.scala
	scalac TestClient.scala

game: Board.scala Player.scala Letterpress.scala
	scalac Board.scala Player.scala Letterpress.scala

server: Server.scala
	scalac Server.scala

trie: Trie.scala TrieBuilder.scala
	scalac Trie.scala TrieBuilder.scala

clean: @$(RM) *.class