import scala.util.parsing.combinator.Parsers

object BooleanParser extends Parsers {
    override type Elem = Token

    def apply(token: Token): Either[BooleanParserError, BooleanAST] = {
        token match {
            case BOOLEAN_VAL(booleanVal) => Right(BooleanValue(booleanVal))
        }
    }
}

sealed trait BooleanAST
final case class BooleanValue(str: String) extends BooleanAST

trait BooleanParserError
