import scala.util.parsing.combinator.Parsers

object BooleanParser extends Parsers {
    override type Elem = Token

    def apply(tokens: List[Token]): Either[BooleanParserError, BooleanAST] = {
        tokens.head match {
            case BOOLEAN_VAL(booleanVal) => Right(BooleanValue(booleanVal))
        }
    }
}

sealed trait BooleanAST
final case class BooleanValue(str: String) extends BooleanAST

trait BooleanParserError
