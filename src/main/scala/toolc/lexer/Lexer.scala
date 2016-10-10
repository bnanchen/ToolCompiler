package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  /** Maps a string s to the corresponding keyword,
    * or None if it corresponds to no keyword
    */
  private def keywords(s: String): Option[Token] = s match {
    case "program"  => Some(PROGRAM())
    //case _          => ??? // TODO
    case "class" => Some(CLASS())
    case "def" => Some(DEF())
    case "var" => Some(VAR())
    case "String" => Some(STRING())
    case "extends" => Some(EXTENDS())
    case "Int" => Some(INT())
    case "Bool" => Some(BOOLEAN())
    case "while" => Some(WHILE())
    case "if" => Some(IF())
    case "else" => Some(ELSE())
    case "return" => Some(RETURN())
    case "length" => Some(LENGTH())
    case "true" => Some(TRUE())
    case "false" => Some(FALSE())
    case "this" => Some(THIS())
    case "new" => Some(NEW())
    case "println" => Some(PRINTLN())
    case "do" => Some(DO())
    case ":" => Some(COLON()) 
    case ";" => Some(SEMICOLON())
    case "." => Some(DOT())
    case "," => Some(COMMA())
    case "==" => Some(EQUALS())
    case "=" => Some(EQSIGN())
    case "!" => Some(BANG())
    case "(" => Some(LPAREN())
    case ")" => Some(RPAREN())
    case "[" => Some(LBRACKET())
    case "]" => Some(RBRACKET())
    case "{" => Some(LBRACE())
    case "}" => Some(RBRACE())
    case "&&" => Some(AND())
    case "||" => Some(OR())
    case "<" => Some(LESSTHAN())
    case "+" => Some(PLUS())
    case "-" => Some(MINUS())
    case "*" => Some(TIMES())
    case "/" => Some(DIV())
    case _ => None
    // comment gérer BAD(), ID(value), INTLIT(value), STRINGLIT(value) and EOF()????
  }


  /** Reads the contents of a file, caching two characters at a time.
    * That way we can have a 2-character lookahead with
    * currentChar and nextChar
    */
  private class SourceReader(f: File) {
    private val source = Source.fromFile(f)

    /** We use this character to mark the end of the input stream. */
    val EndOfFile: Char = java.lang.Character.MAX_VALUE
    
    private var currentChar_ : Char = _
    private var nextChar_ : Char = _
    private var currentPos_ : Positioned = _
    private var nextPos_ : Positioned = _

    /** The current character */
    def currentChar = currentChar_
    /** The next character */
    def nextChar = nextChar_
    /** The position of the current character */
    def currentPos = currentPos_

    private def readChar(): Char = if (source.hasNext) {
      source.next
    } else {
      EndOfFile
    }

    /** Consumes a character from the input.
      * nextChar becomes currentChar,
      * nextChar points to the first unread character.
      */
    def consume() = {
      currentChar_ = nextChar_
      currentPos_ = nextPos_
      nextChar_ = readChar()
      nextPos_ = new Positioned{}.setPos(f, source.pos)
    }

    /** Consume n characters */
    def consume(n: Int): Unit = for (i <- 1 to n) consume()

    // To start, read the first two characters of the file
    consume(2)
  }


  def run(ctx: Context)(f: File): Iterator[Token] = {
    import ctx.reporter._

    val reader = new SourceReader(f)
    import reader._

    /** Gets rid of whitespaces and comments and calls readToken to get the next token. */
    @scala.annotation.tailrec
    def nextToken(): Token = {
      while (Character.isWhitespace(currentChar)) {
        consume()
      }
      if (currentChar == '/' && nextChar == '/') {
        consume(2)
        // Skip until EOL
        while(currentChar != '\n' && currentChar != '\r' && currentChar != EndOfFile) consume()
        nextToken()
      } else if (currentChar == '/' && nextChar == '*') {
        // TODO handle multiline comments. Remember to fail for unclosed comments!
        consume(2)
        while(currentChar != '*' && nextChar != '/' && currentChar != EndOfFile) {
          consume()
        }
        nextToken()
      } else {
        readToken()
      }
    }

    /** Reads the next token from the stream. */
    def readToken(): Token = { // faux ne prend en compte que les éléments peuvent être attaché
      // The position at the beginning of the token.
      val tokenPos = currentPos
      // TODO
      var word: String = currentChar.toString();
      if (currentChar == EndOfFile) {
        val token = EOF()
        token.setPos(tokenPos)
        token
    } else if (currentChar == ':' || currentChar == ';' || currentChar == ',' || currentChar == '!' || currentChar == '(' || currentChar == ')' || currentChar == '['
          || currentChar == ']' || currentChar == '{' || currentChar == '}' || currentChar == '*' || currentChar == '/' || currentChar == '+' || currentChar == '-' 
          || currentChar == '<' || currentChar == '.') {
          consume() 
          val token = keywords(word).getOrElse(BAD())
          token.setPos(tokenPos)
          return token
      } else if (currentChar == '=') {
        if (nextChar == '=') {
          consume(2)
          val token = EQUALS()
          token.setPos(tokenPos)
          return token
        } else {
          consume()
          val token = EQSIGN()
          token.setPos(tokenPos)
          return token
        }
      } else if (currentChar == '"') {  // "Bonjour" currentChar:r  nextChar:" 
        // les erreurs!!!!!!
        consume()
        var w: String = currentChar.toString()
        var bool: Boolean = true
        while(currentChar != '"') { // est-ce que le toChar change qqch? Non
          if (currentChar != '1' && currentChar != '2' && currentChar != '3' && currentChar != '4' && currentChar != '5' && currentChar != '6' 
              && currentChar != '7' && currentChar != '8' && currentChar != '9' && currentChar != '0') {
            bool = false
          }
          consume() // il reste dans la boucle
          w = w + currentChar
        }
        consume()
        if (bool == true) {
          val token = INTLIT(w.slice(0, w.length - 1).toInt)
          token.setPos(tokenPos)
        return token
        } else {
          val token = STRINGLIT(w.slice(0, w.length - 1))
          token.setPos(tokenPos)
        return token
        }
       } else if (currentChar == '&' || currentChar == '|') {
        word + nextChar
        val token = keywords(word).getOrElse(BAD())
        token.setPos(tokenPos)
        return token
      } else { // essaie peut-être un pattern matching. Pense aux automatons!!
        // soit nom de variable soit statement
        // faire un pattern matching avec un acc?? Une sorte d'automaton

        def maybeS(word: String): Token = {
          //consume(2)
          if (currentChar == '(' || Character.isWhitespace(currentChar)) {
            val token = keywords(word).getOrElse(BAD())
            token.setPos(tokenPos)
            return token
          } else if (Character.isWhitespace(nextChar)) {
            word + currentChar
            consume()
            val token = keywords(word).getOrElse(BAD())
            token.setPos(tokenPos)
            return token
          } else {
          accID(word,2)
          }
        }
        
        def maybeW(word: String): Token = {
          //consume(2)
          if (Character.isWhitespace(currentChar)) {
            val token = keywords(word).getOrElse(BAD())
            token.setPos(tokenPos)
            token
          } else {
            accID(word, 3)
          }
        }
         
        def maybeM(word: String): Token = {
          //consume(2)
          println("MAYBEM "+ currentChar)
          if (Character.isWhitespace(currentChar) || currentChar == '{') {
            val token = keywords(word).getOrElse(BAD())
            token.setPos(tokenPos)
            token
          } else if (currentChar == '.' || Character.isWhitespace(currentChar) || currentChar == ';') { // pour this
            println("BOOOOOUM")
            val token = THIS()
            token.setPos(tokenPos)
            token
          } else if (Character.isWhitespace(currentChar) || currentChar == ')' || currentChar == ';') { // pour true
            val token = TRUE()
            token.setPos(tokenPos)
            token
          } else if (Character.isWhitespace(currentChar) || currentChar == ';') {
            val token = BOOLEAN()
            token.setPos(tokenPos)
            token
          }
          else {
            accID(word,4)
          }
        }
        
        def maybeL(word: String): Token = {
          //consume(2)
          if (Character.isWhitespace(currentChar)) {
            val token = keywords(word).getOrElse(BAD())
            token.setPos(tokenPos)
            token
          } else if (Character.isWhitespace(currentChar) || currentChar == ')' || currentChar == ';') { // pour false
            val token = FALSE()
            token.setPos(tokenPos)
            token
          } else {
            accID(word,5)
          }
        }
        
        def maybeF(word: String): Token = {
          //consume(2)
          if (Character.isWhitespace(currentChar) || currentChar == ')' || currentChar == ';') {
            val token = keywords(word).getOrElse(BAD())
            token.setPos(tokenPos)
            token
          } else {
            accID(word,6)
          }
        }
        
        def maybeB(word: String): Token = {
          //consume(2)
          if (Character.isWhitespace(currentChar)) {
            val token = keywords(word).getOrElse(BAD())
            token.setPos(tokenPos)
            token
          } else if (Character.isWhitespace(currentChar) || currentChar == '(') {
            val token = keywords(word).getOrElse(BAD())
            token.setPos(tokenPos)
            token
          } else {
            accID(word,7)
          }
        }
        
        def accID(word: String, l: Int): Token = { // les erreurs??
          if (l == 0) {
            //var w = word.slice(0, word.length-1)
            val token = ID(word)
            token.setPos(tokenPos)
            token
          } else {
          var w = word
          //consume()
          while (!(Character.isWhitespace(currentChar)) && currentChar != ':' && currentChar != ';' && currentChar != ',' && currentChar != '!' && currentChar != '(' && currentChar != ')' && currentChar != '['
          && currentChar != ']' && currentChar != '{' && currentChar != '}' && currentChar != '*' && currentChar != '/' && currentChar != '+' && currentChar != '-' 
          && currentChar != '<' && currentChar != '&' && currentChar != '|' && currentChar != '"') {
            w = w + currentChar
            consume()
          }
          val token = ID(w)
          token.setPos(tokenPos)
          token
          }
        }
        
        def inter(word: String, l: Int): Token = {
          // remplacé tous les nextChar par currentChar dans le if:
          if (Character.isWhitespace(currentChar) || currentChar == ':' || currentChar == ';' || currentChar == ',' || currentChar == '!' || currentChar == '(' || currentChar == ')' || currentChar == '['
          || currentChar == ']' || currentChar == '{' || currentChar == '}' || currentChar == '*' || currentChar == '/' || currentChar == '+' || currentChar == '-' 
          || currentChar == '<' || currentChar == '&' || currentChar == '|' || currentChar == '"') {
            //consume()
            accID(word, 0)
          } else {
            BAD()
          }
        }
        
        
        if (Character.isWhitespace(nextChar) || nextChar == ':' || nextChar == ';' || nextChar == ',' || nextChar == '!' || nextChar == '(' || nextChar == ')' || nextChar == '['
          || nextChar == ']' || nextChar == '{' || nextChar == '}' || nextChar == '*' || nextChar == '/' || nextChar == '+' || nextChar == '-' 
          || nextChar == '<' || nextChar == '&' || nextChar == '|' || nextChar == '"') {
          consume()
          //accID(word, 1)
          val token = inter(word, 0)
          return token
        }
        word = word + nextChar
        consume(2)
        val token1 = word match {
          case "if" => maybeS(word) 
          case "do" => maybeS(word)
          case _ => inter(word, 2)
        }
        
        if (token1 != BAD()) {
          return token1
        }
        
        /*if (Character.isWhitespace(nextChar)) { // 1
          consume()
          accID(word, 1)
        }*/
        //consume()
        if (Character.isWhitespace(currentChar)) { // 2
          //consume()
          accID(word, 2)
        }
        
        word = word + currentChar // nextChar
        consume()
        val token6 = word match {
          case "int" => maybeW(word)
          case "def" => maybeW(word)
          case "var" => maybeW(word)
          case "new" => maybeW(word)
          case _ => BAD()
        }
        if (token6 != BAD()) {
          return token6
        }
        
        //consume()
        if (Character.isWhitespace(currentChar)) { // 3
          //consume()
          accID(word, 3)
        }
        word = word + currentChar //+ nextChar // .length == 4
        consume()
        println("TOKEN2 "+ word)
        val token2 = word match {
          case "else" => maybeM(word)
          case "true" => maybeM(word)
          case "this" => maybeM(word)
          case "Bool" => maybeM(word)
          case _ => BAD()
        }
        if (token2 != BAD()) {
          return token2
        }
        
        //onsume()
        if (Character.isWhitespace(currentChar)) { // 4
          //consume()
          accID(word, 4)
        }
        word = word + currentChar //nextChar // .length == 5
        consume()
        val token3 = word match {
          case "class" => maybeL(word)
          case "false" => maybeL(word) 
          case _ => BAD()
        }
        if (token3 != BAD()) {
          return token3
        }
        //consume()
        if (Character.isWhitespace(currentChar)) { // 5
          //consume()
          accID(word, 5)
        }
        word = word + currentChar //nextChar // .length == 6
        consume()
        val token4 = word match {
          case "String" => maybeF(word) 
          case "return" => maybeF(word)
          case "length" => maybeF(word)
          case _ => BAD()
        }
        if (token4 != BAD()) {
          return token4
        }
        //consume()
        if (Character.isWhitespace(currentChar)) { // 6
          //consume()
          accID(word, 6)
        }
        word = word + currentChar //nextChar // .length == 7
        consume()
        val token5 = word match {
          case "extends" => maybeB(word)
          case "println" => maybeB(word)
          case "program" => maybeB(word)
          case _ => {
            word = word + currentChar
            consume()
            accID(word, 7)
          }
        }
        return token5
    }
    }

    new Iterator[Token] {
      var tokenCache: Token = nextToken()
      var reachedEnd = false

      def hasNext = !reachedEnd

      def next = {
        val r = tokenCache
        if (r == EOF()) {
          reachedEnd = true
        } else {
          tokenCache = nextToken()
        }
        r
      }
    }
  }
}

/** Reads and displays the tokens, then returns a fresh iterator with the same tokens. */
object DisplayTokens extends Pipeline[Iterator[Token], Iterator[Token]] {
  def run(ctx: Context)(tokens: Iterator[Token]): Iterator[Token] = {
    val l = tokens.toList
    l foreach { t => println(s"$t(${t.line}:${t.col})") }
    l.iterator
  }
}