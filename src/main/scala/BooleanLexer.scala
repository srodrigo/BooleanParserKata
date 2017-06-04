import scala.util.parsing.combinator.RegexParsers

sealed trait Token
final case class BOOLEAN_VAL(str: String) extends Token
case object NEGATION_OP extends Token

object BooleanLexer extends RegexParsers {
    def apply(code: String): Either[BooleanLexerError, List[Token]] = {
        parse(tokens, code) match {
            case NoSuccess(msg, _) => Left(BooleanLexerError(msg))
            case Success(result, _) => Right(result)
        }
    }

    def tokens: Parser[List[Token]] = {
        phrase(rep1(booleanVal("T") | booleanVal("F") | negationOp))
    }

    private def booleanVal(bool: String) = {
        "[FT]".r ^^ { str => BOOLEAN_VAL(str) }
    }

    private def negationOp = {
        "NOT".r ^^ { _ => NEGATION_OP }
    }
}
