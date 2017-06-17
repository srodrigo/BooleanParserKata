import scala.util.parsing.combinator.RegexParsers

sealed trait Token

case object TRUE_VAL extends Token
case object FALSE_VAL extends Token
case object NOT_OP extends Token
case object AND_OP extends Token
case object OR_OP extends Token
case object OPEN_PAR extends Token
case object CLOSE_PAR extends Token


object BooleanLexer extends RegexParsers {

    private val TRUE = "T"
    private val FALSE = "F"

    def apply(code: String): Either[String, List[Token]] =
        parse(tokens, code) match {
            case NoSuccess(msg, _) => Left(msg)
            case Success(result, _) => Right(result)
        }

    def tokens: Parser[List[Token]] =
        phrase(rep1(booleanVal(TRUE) | booleanVal(FALSE) | negationOp | andOp | orOp | openPar | closePar))

    private def booleanVal(bool: String) =
        ("[" + FALSE + TRUE + "]").r ^^ { str =>
            if (str == TRUE) TRUE_VAL
            else FALSE_VAL
        }

    private val negationOp =
        "NOT".r ^^ { _ => NOT_OP }

    private val andOp =
        "AND".r ^^ { _ => AND_OP }

    private val orOp =
        "OR".r ^^ { _ => OR_OP }

    private val openPar =
        "\\(".r ^^ { _ => OPEN_PAR}

    private val closePar =
        "\\)".r ^^ { _ => CLOSE_PAR}

}
