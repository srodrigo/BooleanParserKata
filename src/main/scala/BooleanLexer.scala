import scala.util.parsing.combinator.RegexParsers

sealed trait Token
final case class BOOLEAN_VAL(bool: Boolean) extends Token
case object NEGATION_OP extends Token
case object AND_OP extends Token


object BooleanLexer extends RegexParsers {

    private val TRUE = "T"
    private val FALSE = "F"

    def apply(code: String): Either[BooleanLexerError, List[Token]] = {
        parse(tokens, code) match {
            case NoSuccess(msg, _) => Left(BooleanLexerError(msg))
            case Success(result, _) => Right(result)
        }
    }

    def tokens: Parser[List[Token]] = {
        phrase(rep1(booleanVal(TRUE) | booleanVal(FALSE) | negationOp))
    }

    private def booleanVal(bool: String) = {
        ("[" + FALSE + TRUE + "]").r ^^ { str =>
            if (str == TRUE) BOOLEAN_VAL(true)
            else BOOLEAN_VAL(false)
        }
    }

    private def negationOp = {
        "NOT".r ^^ { _ => NEGATION_OP }
    }
}
