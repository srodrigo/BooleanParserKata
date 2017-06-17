import scala.util.parsing.combinator.RegexParsers

sealed trait Token
case object TRUE_VAL extends Token
case object FALSE_VAL extends Token
case object NEGATION_OP extends Token
case object AND_OP extends Token
case object OR_OP extends Token


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
        phrase(rep1(booleanVal(TRUE) | booleanVal(FALSE) | negationOp | andOp | orOp))
    }

    private def booleanVal(bool: String) = {
        ("[" + FALSE + TRUE + "]").r ^^ { str =>
            if (str == TRUE) TRUE_VAL
            else FALSE_VAL
        }
    }

    private val negationOp = {
        "NOT".r ^^ { _ => NEGATION_OP }
    }

    private val andOp = {
        "AND".r ^^ { _ => AND_OP }
    }

    private val orOp = {
        "OR".r ^^ { _ => OR_OP }
    }
}
