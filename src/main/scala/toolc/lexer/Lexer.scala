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
    case "string" => Some(STRING())
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
    def readToken(): Token = {
      // The position at the beginning of the token.
      val tokenPos = currentPos
      // TODO
      val word: String = currentChar.toString();
      if (currentChar == '"') {
          consume()
          while(nextChar != '"' || nextChar != '\r') {
            word + currentChar
            consume()
          }
          return STRINGLIT(word)
        }
      while(!Character.isWhitespace(nextChar)) {
            word + currentChar
            consume()
      }
      val optionToken: Option[Token] = keywords(word)
      optionToken match {
        case Some(t) => t
        case None => ID(word)
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
