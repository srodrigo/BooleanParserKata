import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object BooleanParser extends Parsers {
    override type Elem = Token

    def apply(tokens: List[Token]): Either[BooleanParserError, BooleanAST] = {
        println(tokens)
        val reader = new BooleanTokenReader(tokens)
        val result = expression(reader) match {
            case NoSuccess(msg, _) => Left(BooleanParserError(msg))
            case Success(result, _) => Right(result)
        }
        println(result)
        result
    }

    private def expression: Parser[BooleanAST] = {
        phrase(block)
    }

    private def block: Parser[BooleanAST] = {
        rep1(booleanExpr) ^^ (exprList => exprList reduceRight BooleanExpr)
    }

    private def booleanExpr: Parser[BooleanAST] = {
        negatedBooleanValue | andOp | orOp | booleanValue
    }

    private def trueValue: Parser[BooleanAST] = TRUE_VAL ^^^ TrueValue
    private def falseValue: Parser[BooleanAST] = FALSE_VAL ^^^ FalseValue
    private def booleanValue: Parser[BooleanAST] = trueValue | falseValue

    private val negatedBooleanValue = NEGATION_OP ~ booleanExpr ^^ {
        case negationOp ~ booleanExpr => NotOp(booleanExpr)
    }

    private val andOp = booleanValue ~ AND_OP ~ booleanExpr ^^ {
        case left ~ op ~ right => AndOp(left, right)
    }

    private val orOp = booleanValue ~ OR_OP ~ booleanExpr ^^ {
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
