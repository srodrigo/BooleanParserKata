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
        rep1(booleanExpr) ^^ (exprList => exprList reduceRight BooleanExpr)
    }

    private def booleanExpr: Parser[BooleanAST] = {
        negatedBooleanValueAST | andOp | orOp | booleanValue
    }

    private def trueValue: Parser[BooleanAST] = TRUE_VAL ^^^ TrueValue
    private def falseValue: Parser[BooleanAST] = FALSE_VAL ^^^ FalseValue
    private def booleanValue: Parser[BooleanAST] = trueValue | falseValue

    private val negatedBooleanValueAST = NEGATION_OP ~ booleanValue ^^ {
        case negationOp ~ booleanValue => NotOp(booleanValue)
    }

    private val andOp = booleanValue ~ AND_OP ~ booleanValue ^^ {
        case left ~ op ~ right => AndOp(left, right)
    }

    private val orOp = booleanValue ~ OR_OP ~ booleanValue ^^ {
        case left ~ op ~ right => OrOp(left, right)
    }
}

sealed trait BooleanAST

sealed trait BooleanValue extends BooleanAST
case object TrueValue extends BooleanValue
case object FalseValue extends BooleanValue

final case class AndOp(left: BooleanAST, right: BooleanAST) extends BooleanAST
final case class OrOp(left: BooleanAST, right: BooleanAST) extends BooleanAST
final case class NotOp(expr: BooleanAST) extends BooleanAST
final case class BooleanExpr(step1: BooleanAST, step2: BooleanAST) extends BooleanAST

class BooleanTokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[Token] = new BooleanTokenReader(tokens.tail)
}
