import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object BooleanParser extends Parsers {
    override type Elem = Token

    def apply(tokens: List[Token]): Either[BooleanParserError, BooleanAST] = {
        val reader = new BooleanTokenReader(tokens)
        expression(reader) match {
            case NoSuccess(msg, _) => Left(BooleanParserError(msg))
            case Success(result, _) => Right(result)
        }
    }

    private def expression: Parser[BooleanAST] = {
        phrase(block)
    }

    private def block: Parser[BooleanAST] = {
        rep1(booleanExpr) ^^ { case stmtList => stmtList reduceRight BooleanExpr }
    }

    private def booleanExpr: Parser[BooleanAST] = {
        booleanValue ^^ (booleanVal => BooleanValue(booleanVal.str))
    }

    private def booleanValue: Parser[BOOLEAN_VAL] = {
        accept(
            "boolean_value",
            { case booleanVal @ BOOLEAN_VAL(_) => booleanVal }
        )
    }
}

sealed trait BooleanAST
final case class BooleanValue(str: String) extends BooleanAST
final case class BooleanExpr(step1: BooleanAST, step2: BooleanAST) extends BooleanAST

class BooleanTokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[Token] = new BooleanTokenReader(tokens.tail)
}
