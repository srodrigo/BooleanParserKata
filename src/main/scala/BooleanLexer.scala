import scala.util.parsing.combinator.RegexParsers

sealed trait Token
final case class BOOLEAN_VAL(str: String) extends Token

object BooleanLexer extends RegexParsers {
    def apply(code: String): Either[BooleanLexerError, List[Token]] = {
        parse(tokens, code) match {
            case NoSuccess(msg, _) => Left(BooleanLexerError(msg))
            case Success(result, _) => Right(result)
        }
    }

    def tokens: Parser[List[Token]] = {
        phrase(rep1(booleanVal("T") | booleanVal("F")))
    }

    def booleanVal(bool: String): Parser[BOOLEAN_VAL] = {
        "[FT]".r ^^ { str => BOOLEAN_VAL(str) }
    }
}

trait BooleanCompilationError
case class BooleanLexerError(msg: String) extends BooleanCompilationError
